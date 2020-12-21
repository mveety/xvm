#include <u.h>
#include <cpu.h>
#include <insts.h>

int
doanint(Cpu *c, u8int intn, int rval)
{
	if(!(c->regs[regFL] & 1<<2)){
instfail1:
		c->halted = 1;
		return ErrUnableToInt;
	}
	c->regs[regSP] += 4;
	if(memwrite32(c, c->regs[regSP], c->regs[regPC]))
		goto instfail1;
	c->regs[regSP] += 4;
	if(memwrite32(c, c->regs[regSP], c->regs[regFL]))
		goto instfail1;
	c->regs[regPC] = c->regs[regFV];
	c->regs[regFL] &= ~(1<<0);
	c->regs[regFL] &= ~(1<<1);
	c->regs[regFL] &= ~(0xf<<4);
	c->regs[regFL] |= intn<<4;
	return rval;
}

// cpuexec fully executes a single instruction on each call
int
cpuexec(Cpu *c)
{
	RawInst rcurinst;
	Inst curinst;
	u8int tmp;

	if(c->halted)
		return 0;	// dunno man
	if(c->doint){
		tmp = c->intn;
		c->doint = 0;
		c->intn = 0;
		return doanint(c, tmp, 0);
	}
	if(fetch(c, &rcurinst))
		return doanint(c, FaultMem, ErrMem);
	if(decode(c, &rcurinst, &curinst))
		return doanint(c, FaultInst, ErrInvalidInst);
	if(cpuexec1(c, &curinst))
		return doanint(c, FaultMem, ErrMem);
	c->regs[regPC] = c->nextpc;
	return 0;
}

