#include "lj_fopen.h"
/*
 * Forces to look for unicode paths on Windows
 */
#ifdef _WIN32
#include <windows.h>

FILE *_lua_fopen(const char *filename, const char *mode)
{
    int fn_len_s=strlen(filename);
    if(fn_len_s==0) return NULL;
    int m_len_s=strlen(mode);
    if(m_len_s==0) return NULL;
    wchar_t path[MAX_PATH];
    wchar_t wmode[MAX_PATH];
    int new_Len1 = MultiByteToWideChar(CP_UTF8, 0, filename, fn_len_s, path, fn_len_s);
    if(new_Len1>=MAX_PATH) return NULL;
    path[new_Len1] = L'\0';
    int new_Len2 = MultiByteToWideChar(CP_UTF8, 0, mode, m_len_s, wmode, m_len_s);
    if(new_Len2>=MAX_PATH) return NULL;
    wmode[new_Len2] = L'\0';
    FILE *f = _wfopen(path, wmode);
    return f;
}

FILE *_lua_freopen(const char *filename, const char *mode, FILE * oldfile)
{
    int fn_len_s=strlen(filename);
    if(fn_len_s==0) return NULL;
    int m_len_s=strlen(filename);
    if(m_len_s==0) return NULL;
    wchar_t path[MAX_PATH];
    wchar_t wmode[MAX_PATH];
    int new_Len1 = MultiByteToWideChar(CP_UTF8, 0, filename, fn_len_s, path, fn_len_s);
    path[new_Len1] = L'\0';
    int new_Len2 = MultiByteToWideChar(CP_UTF8, 0, mode, m_len_s, wmode, m_len_s);
    wmode[new_Len2] = L'\0';
    FILE *f = _wfreopen(path, wmode, oldfile);
    LocalFree(path);
    LocalFree(wmode);
    return f;
}

#endif
