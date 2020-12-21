typedef struct MemMap MemMap;
typedef struct Sysman Sysman;

struct SysMan {
	// some control fields
	u8int mode;
	int nvramfd;
	// memory
	u8int *initrom;
	u8int *nvram;
	u8int *bootrom;
	u8int *smram;
};

struct MemMap {
	u32int start;
	u32int *end; // could be alias to me->next->start
	u32int size;
	u8int type;
	// both of these take addresses referenced from start
	// ie realaddr = givenaddr - start
	u8int (*memread)(u32int);
	void (*memwrite)(u32int, u8int);
	u8int *memptr;
	MemMap *next;
};

extern MemMap *memmap;
extern MemMap *sysmanmap;
extern MemMap *lowram;
extern MemMap *highram;

extern u8int mmread(u32int);
extern void mmwrite(u32int, u8int);
extern int mmtest(u32int);

extern MemMap *findmapping(u32int);


