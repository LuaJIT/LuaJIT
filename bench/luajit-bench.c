/* Benchmark driver.
 *
 * Copyright (C) 2019 Vlad Krasnov
 * Copyright (C) 2019 Siddhesh Poyarekar
 *
 * The driver was originally written by Vlad Krasnov as part of the bench_lua
 * project:
 *
 *   https://github.com/vkrasnov/bench_lua
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include <inttypes.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/mman.h>
#include <argp.h>
#include <sys/param.h>
#include <string.h>
#include <time.h>

#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"

typedef struct {
  uint64_t ctr;
  const char *script;
  const char **args;
  uint64_t start_ns;
  uint64_t end_ns;
} lua_task;

static int arg_num = 0;

/* Benchmark execution duration in seconds. */
#ifndef BENCH_DURATION
#define BENCH_DURATION 10
#endif

#define TIMESPEC_NS(t) ((t).tv_sec * 1e9 + (t).tv_nsec)

void *wrapper(void *arg)
{
  lua_task *task = (lua_task *) arg;
  struct timespec start_time;
  struct timespec cur_time;
  uint64_t ctr = 0;

  int status = clock_gettime(CLOCK_MONOTONIC, &start_time);

  if (status != 0) {
    fprintf(stderr, "clock_gettime failed: %s\n", strerror(errno));
    exit(1);
  }

  uint64_t cur_time_ns = TIMESPEC_NS (start_time);
  uint64_t stop_time_ns = TIMESPEC_NS (start_time) + BENCH_DURATION * 1e9;

  lua_State *L;
  L = luaL_newstate();
  luaL_openlibs(L);
  char script_name[64];

  snprintf(script_name, sizeof (script_name), "%s.lua", task->script);

  status = luaL_loadfile(L, script_name);
  if (status) {
    fprintf(stderr, "Couldn't load script: %s\n", lua_tostring(L, -1));
    exit(1);
  }

  int cb_ref = luaL_ref(L, LUA_REGISTRYINDEX);

  do {
    lua_rawgeti(L, LUA_REGISTRYINDEX, cb_ref);
    for (int i = 0; i < arg_num; i++)
      lua_pushstring(L, task->args[i]);

    int result = lua_pcall(L, arg_num, 0, 0);
    if (result) {
      fprintf(stderr, "Failed to run script: %s\n", lua_tostring(L, -1));
      exit(1);
    }
    ctr++;

    status = clock_gettime(CLOCK_MONOTONIC, &cur_time);
    if (status != 0) {
      fprintf(stderr, "clock_gettime failed: %s\n", strerror(errno));
      exit(1);
    }
    cur_time_ns = TIMESPEC_NS(cur_time);
  } while (stop_time_ns > cur_time_ns);

  task->ctr = ctr;
  task->start_ns = TIMESPEC_NS(start_time);
  task->end_ns = cur_time_ns;

  return NULL;
}

#define ARG_MAX 3
typedef struct {
  int c, q, d, b;
  const char *args[ARG_MAX];
  char *script_name;
} cmd_line_options;

static void args_append(const char **args, char *arg)
{
  if (arg_num >= ARG_MAX) {
    fprintf(stderr, "Maximum of %d arguments allowed for a benchmark.",
            ARG_MAX - 1);
    exit(1);
  }

  args[arg_num++] = arg;
}

const char *argp_program_version = "0.01";
const char *argp_program_bug_address = "<luajit@freelists.org>";

/* Program documentation. */
static char doc[] = "Runs lua on multiple threads";

/* A description of the arguments we accept. */
static char args_doc[] = "script";

static struct argp_option options[] = {
  {"concurrency", 'c', "concurrency", 0, "Number of threads"},
  {"arg", 'a', "arg", 0, "Argument to pass to the benchmark. "
                         "Repeat to add multiple arguments."},
  {0}
};

static error_t parse_opt(int key, char *arg, struct argp_state *state)
{
  cmd_line_options *arguments = state->input;
  switch (key) {
  case 'c':
    arguments->c = atoi(arg);
    break;

  case 'a':
    args_append(arguments->args, arg);
    break;

  case ARGP_KEY_ARG:
    if (state->arg_num >= 1) {
      argp_usage(state);
    }
    arguments->script_name = arg;
    break;

  case ARGP_KEY_END:
    if (state->arg_num < 1) {
      argp_usage(state);
    }
    break;

  default:
    return ARGP_ERR_UNKNOWN;
  }
  return 0;
}

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc };

int main(int argc, char *argv[])
{
  pthread_t *threads;
  lua_task *tasks;
  int i;
  uint8_t *buf;
  size_t len;
  struct stat s;
  uint64_t total = 0;

  cmd_line_options arguments = (cmd_line_options) {
    .script_name = "",
    .q = 8,
    .c = 1
  };

  int ret = argp_parse(&argp, argc, argv, 0, 0, &arguments);

  if (ret != 0) {
    fprintf(stderr,
            "Unexpected error while parsing command line arguments: %s\n",
            strerror(ret));
    return 1;
  }

  /* Ignore all benchmark prints to stdout.  We retain stdout so that we
   * can print the score in the end. */
  int out_copy = dup(1);
  close(1);
  int out = open ("/dev/null", O_WRONLY | O_CLOEXEC);

  if (out < 0 || out != 1) {
    fprintf (stderr, "Failed to open /dev/null as stdout(%d): %s\n", out,
             strerror (errno));

    return 1;
  }

  int nthreads = arguments.c;
  threads = malloc(nthreads * sizeof(pthread_t));
  tasks = malloc(nthreads * sizeof(lua_task));

  if (threads == NULL || tasks == NULL) {
    fprintf(stderr, "Unable to allocate memory: %s\n", strerror(errno));
    return 1;
  }

  for (i = 0; i < nthreads; i++) {
    tasks[i] = (lua_task) {
      .script = arguments.script_name,
      .args = arguments.args
    };

    int ret = pthread_create(&threads[i], NULL, wrapper, &tasks[i]);

    if (ret != 0) {
      fprintf(stderr, "Cannot create thread(%d): %s\n", i, strerror(errno));
      return 1;
    }
  }

  uint64_t start_time_ns = 0;
  uint64_t end_time_ns = 0;

  for (i = 0; i < nthreads; i++) {
    int ret = pthread_join(threads[i], NULL);

    if (ret != 0) {
      fprintf(stderr, "Thread join failed(%d): %s\n", i, strerror(errno));
      return 1;
    }

    total += tasks[i].ctr;
    /* Select the earliest starting time and the latest end time. */
    if (start_time_ns == 0) {
      start_time_ns = tasks[i].start_ns;
      end_time_ns = tasks[i].end_ns;
    } else {
      start_time_ns = MIN(start_time_ns, tasks[i].start_ns);
      end_time_ns = MAX(end_time_ns, tasks[i].end_ns);
    }
  }

  uint64_t duration = end_time_ns - start_time_ns;

  char output[256];
  snprintf(output, sizeof (output), "%s: %" PRId64 ": %.4f\n",
           arguments.script_name, total, (double)total * 1e9 / duration);

  write (out_copy, output, strlen (output));

  return 0;
}
