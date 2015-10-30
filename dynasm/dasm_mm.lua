
--wrappers around mmap to support dynamic code exection.
--Written by Cosmin Apreutesei. Public Domain.
--Tested with Windows, Linux and OSX, x86 and x86-64.

local ffi = require'ffi'
local C = ffi.C

local function checkh(ptr) return assert(ptr ~= nil and ptr) end
local function checkz(ret) assert(ret == 0) end
local function checknz(ret) assert(ret ~= 0) end

local new, free, protect

--Using VirtualAlloc allows memory protection, but can only allocate memory in multiple-of-64K chunks.
local USE_VIRTUALALLOC = false

if ffi.os == 'Windows' then

	if USE_VIRTUALALLOC then

		ffi.cdef[[
		void* VirtualAlloc(void* lpAddress, size_t dwSize, uint32_t flAllocationType, uint32_t flProtect);
		int VirtualFree(void* lpAddress, size_t dwSize, uint32_t dwFreeType);
		int VirtualProtect(void* lpAddress, size_t dwSize, uint32_t flNewProtect, uint32_t* lpflOldProtect);
		]]

		local PAGE_READWRITE    = 0x04
		local PAGE_EXECUTE_READ = 0x20
		local MEM_COMMIT  = 0x1000
		local MEM_RESERVE = 0x2000
		local MEM_RELEASE = 0x8000

		function new(size)
			return checkh(C.VirtualAlloc(nil, size, bit.bor(MEM_RESERVE, MEM_COMMIT), PAGE_READWRITE))
		end

		function protect(addr, size)
			local oldprotect = ffi.new'uint32_t[1]' --because null not accepted
			checknz(C.VirtualProtect(addr, size, PAGE_EXECUTE_READ, oldprotect))
		end

		function free(addr, size)
			assert(size, 'size required') --on other platforms
			checknz(C.VirtualFree(addr, 0, MEM_RELEASE))
		end

	else

		local HEAP_NO_SERIALIZE          = 0x00000001
		local HEAP_ZERO_MEMORY           = 0x00000008
		local HEAP_CREATE_ENABLE_EXECUTE = 0x00040000

		ffi.cdef[[
		void* HeapCreate(uint32_t flOptions, size_t dwInitialSize, size_t dwMaximumSize);
		void* HeapAlloc(void* hHeap, uint32_t dwFlags, size_t dwBytes);
		int HeapFree(void* hHeap, uint32_t dwFlags, void* lpMem);
		]]

		local heap

		function new(size)
			heap = heap or checkh(C.HeapCreate(bit.bor(HEAP_NO_SERIALIZE, HEAP_CREATE_ENABLE_EXECUTE), 0, 0))
			return checkh(C.HeapAlloc(heap, HEAP_ZERO_MEMORY, size))
		end

		function protect(addr, size) end

		function free(addr, size)
			assert(size, 'size required') --on other platforms
			checknz(C.HeapFree(heap, HEAP_NO_SERIALIZE, addr))
		end

	end

elseif ffi.os == 'Linux' or ffi.os == 'OSX' then

	if ffi.os == 'OSX' then
		ffi.cdef'typedef int64_t off_t;'
	else
		ffi.cdef'typedef long int off_t;'
	end

	ffi.cdef[[
	void* mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
	int munmap(void *addr, size_t length);
	int mprotect(void *addr, size_t len, int prot);
	]]

	local PROT_READ  = 1
	local PROT_WRITE = 2
	local PROT_EXEC  = 4
	local MAP_PRIVATE   = 2
	local MAP_ANON = ffi.os == 'Linux' and 0x20 or 0x1000

	function new(size)
		local ret = C.mmap(nil, size, bit.bor(PROT_READ, PROT_WRITE), bit.bor(MAP_PRIVATE, MAP_ANON), -1, 0)
		if ffi.cast('intptr_t', ret) == ffi.cast('intptr_t', -1) then
			error(string.format('mmap errno: %d', ffi.errno()))
		end
		return checkh(ret)
	end

	function protect(addr, size)
		checkz(C.mprotect(addr, size, bit.bor(PROT_READ, PROT_EXEC)))
	end

	function free(addr, size)
		checkz(C.munmap(addr, size))
	end

end

local new = function(size) --override for hooking to gc
	local addr = new(size)
	ffi.gc(addr, function(addr)
		free(addr, size)
		ffi.gc(addr, nil)
	end)
	return addr
end

if not ... then
	local function test(size)
		local addr = new(size)
		print(addr)
		addr = ffi.cast('int32_t*', addr)
		assert(addr[0] == 0)
		addr[0] = 1234 --writable
		assert(addr[0] == 1234)
		protect(addr, size)
		--addr[0] = 4321 --uncomment this to get a crash (r/o memory); TODO: test if executable
		--addr = nil; collectgarbage() --enable this to fail the assertion below
		return addr
	end
	local a1 = test(64*1024*1000) --64MB
	local a2 = test(16) --16 bytes
	assert(a1 ~= a2) --different pages
	a1 = nil
	a2 = nil
	collectgarbage() --TODO: test if finalizer was called
end

return {new = new, free = free, protect = protect}
