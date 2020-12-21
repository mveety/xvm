#include <u.h>
#include <cpu.h>

u8int
devread8(Cpu *c, u32int port)
{
	u32int tmp;

	tmp = c->devread(port);
	return (u8int)(tmp & 0xff);
}

u16int
devread16(Cpu *c, u32int port)
{
	u32int tmp;

	tmp = c->devread(port);
	return (u16int)(tmp & 0xffff);
}

u32int
devread32(Cpu *c, u32int port)
{
	return c->devread(port);
}

void
devint(Cpu *c, u32int port)
{
	c->devint(port);
}

void
devwrite8(Cpu *c, u32int port, u8int data)
{
	u32int tmp = 0;

	tmp = (u32int)data;
	c->devwrite(port, tmp);
}

void
devwrite16(Cpu *c, u32int port, u16int data)
{
	u32int tmp = 0;

	tmp = (u32int)data;
	c->devwrite(port, tmp);
}

void
devwrite32(Cpu *c, u32int port, u32int data)
{
	c->devwrite(port, data);
}

