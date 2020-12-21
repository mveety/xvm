#include <u.h>
#include <cpu.h>

u32int addrdecode(Cpu *c, u32int addr)
{
	if(!addrmode(c))
		return addr + OP(c);
	return addr;
}

int
memcheck(Cpu *c, u32int addr, u32int size)
{
	u32int i;

	for(i = 0; i < size; i++)
		if(addr + i < c->memmin || c->memmax < addr + i)
			return -1;
	return 0;
}

int
memread(Cpu *c, u32int addr, u8int *dat)
{
	if(addr < c->memmin || c->memmax < addr){
		c->hwint(FaultMem);
		return -1;
	}
	*d = c->memread(addr);
	return 0;
}

int
memwrite(Cpu *c, u32int addr, u8int dat)
{
	if(addr < c->memmin || c->memmax < addr){
		c->hwint(FaultMem);
		return -1;
	}
	c->memwrite(addr, dat);
	return 0;
}

int
memread16(Cpu *c, u32int addr, u16int *dat)
{
	u8int b[2];
	u16int tmp;

	if(memcheck(c, addr, 2)){
		c->hwint(FaultMem);
		return -1;
	}
	memread(c, addr, &b[0]);
	memread(c, addr+1, &b[1]);
	tmp = (b[0]<<8) | b[1];
	*dat = tmp;
	return 0;
}

int
memwrite16(Cpu *c, u32int addr, u16int dat)
{
	u8int b[2];

	if(memcheck(c, addr, 2)){
		c->hwint(FaultMem);
		return -1;
	}
	b[0] = (u8int)(dat>>8 & 0xff);
	b[1] = (u8int)(dat & 0xff);
	memwrite(c, addr, b[0]);
	memwrite(c, addr+1, b[1]);
	return 0;
}

int
memread32(Cpu *c, u32int addr, u32int *dat)
{
	u8int b[4];
	u32int tmp;

	if(memcheck(c, addr, 2)){
		c->hwint(FaultMem);
		return -1;
	}
	memread(c, addr, &b[0]);
	memread(c, addr+1, &b[1]);
	memread(c, addr+2, &b[2]);
	memread(c, addr+3, &b[3]);
	tmp = (b[0]<<24) | (b[1]<<16) | (b[2]<<8) | b[3];
	*d = tmp;
	return 0;
}

int
memwrite32(Cpu *c, u32int addr, u32int dat)
{
	u8int b[4];

	if(memcheck(c, addr, 2)){
		c->hwint(FaultMem);
		return -1;
	}
	b[0] = (u8int)((dat>>24) & 0xff);
	b[1] = (u8int)((dat>>16) & 0xff);
	b[2] = (u8int)((dat>>8) & 0xff);
	b[3] = (u8int)(dat & 0xff);
	memwrite(c, addr, b[0]);
	memwrite(c, addr+1, b[1]);
	memwrite(c, addr+2, b[2]);
	memwrite(c, addr+3, b[3]);
	return 0;
}

