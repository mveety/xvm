#include <u.h>
#include <cpu.h>
#include <instr.h>

// note:
// cpuexec1 does the exec, mem write, and reg write stages of the pipeline
// some instructions (specifically ldi, push, pusha, pop, and popa) require
// chained memory reads and writes which prevent easy splitting of the pipeline
// into multiple function.

void
regfetch(Cpu *c, Inst *inst, u32int *regdata)
{
	if(inst->usereg == 0)
		return;
	regdata[0] = c->regs[inst->argregs[0]];
	if(inst->usereg == 2)
		regdata[1] = c->regs[inst->argregs[1]];
}

void
regwrite(Cpu *c, Inst *inst, u32int *regdata)
{
	if(inst->usereg == 0)
		return;
	c->regs[inst->argregs[0]] = regdata[0];
	if(inst->usereg == 2)
		c->regs[inst->argregs[1]] = regdata[1];
}

int
cpuexec1(Cpu *c, Inst *inst)
{
	// iterator
	// int i = 0;
	// register scratch space
	u32int regdata[2] = {0, 0};
	// address scratch space
	u32int raddr = 0;
	// accumulator scratch registers
	u8int scratch8 = 0;
	u16int scratch16 = 0;
	u32int scratch = 0;
	// pc scratch register
	u32int newpc = 0;
	u32int newsp = 0;
	// writeback flags
	u8int writescratch = 0; // 1 = 8bit, 2 = 16bit, 4 = 32bit, 0 = no write
	u8int writepc = 0;
	u8int writesp = 0;
	u8int setcond = 0;
	u8int unsetcond = 0;
	u8int dopusha = 0;
	u8int dopopa = 0;
	u8int dosyscall = 0;
	u8int dosysret = 0;
	u8int doiret = 0;
	u8int doint = 0;
	u8int dosint = 0;
	u8int docint = 0;

	// conditional execution
	// if inst->condition is true (meaning mode field is 0xf) then check
	// to see if FL:3 is true. If FL:3 is not true then return 0 to skip
	// the instruction.
	if(inst->condition)
		if(!(cpu->regs[regFL] & 1<<3))
			return 0;

	regfetch(c, inst, &regdata[0]);
	switch(inst->opcode){
	case LDR:
		switch(inst->mode){
		case 0x1:
			raddr = regdata[1];
			break;
		case 0x2:
			raddr = regdata[1] + OP(c);
			break;
		default:
			raddr = addrdecode(c, regdata[1]);
			break;
		}
		goto ldcommon;
		break;
	case LD:
		switch(inst->mode){
		case 0x1:
			raddr = inst->argliteral;
			break;
		case 0x2:
			raddr = inst->argliteral + OP(c);
			break;
		default:
			raddr = addrdecode(c, inst->argliteral);
		}
ldcommon:
		switch(inst->width){
		case 0x1:
			if(memread8(c, raddr, &scratch8))
				return ErrMem;
			regdata[0] = scratch8;
			break;
		case 0x2:
			if(memread16(c, raddr, &scratch16))
				return ErrMem;
			regdata[0] = scratch16;
			break;
		case 0x3:
		default:
			if(memread32(c, raddr, &regdata[0]))
				return ErrMem;
			break;
		}
		break;
	case LDC:
		regdata[0] = argliteral;
		break;
	case LDI:
		switch(inst->mode){
		case 0x1:
			raddr = inst->argliteral;
			break;
		case 0x2:
			raddr = inst->argliteral + OP(c);
			break;
		default:
			raddr = addrdecode(c, inst->argliteral);
			break;
		}
		if(memread32(c, raddr, &raddr))
			return ErrMem;
		switch(inst->mode){
		case 0x1:
			break;
		case 0x2:
			raddr += OP(c);
			break;
		default:
			raddr = addrdecode(c, raddr);
			break;
		}
		goto ldcommon;
		break;
	case STO:
		switch(inst->mode){
		case 0x1:
			raddr = inst->argliteral;
			break;
		case 0x2:
			raddr = inst->argliteral+OP(c);
			break;
		default:
			raddr = addrdecode(c, inst->argliteral);
			break;
		}
stocommon:
		switch(inst->width){
		case 0x1:
			scratch8 = (u8int)(regdata[0] & 0x000000ff);
			writescratch = 1;
			break;
		case 0x2:
			scratch16 = (u16int)(regdata[0] & 0x0000ffff);
			writescratch = 2;
			break;
		case 0x3:
			scratch = regdata[0];
			writescratch = 4;
			break;
		}
		break;
	case STOR:
		switch(inst->mode){
		case 0x1:
			raddr = regdata[1];
			break;
		case 0x2:
			raddr = regdata[1] + OP(c);
			break;
		default:
			raddr = addrdecode(c, regdata[1]);
		}
		goto stocommon;
		break;
	case STOI:
		switch(inst->mode){
		case 0x1:
			raddr = inst->argliteral;
			break;
		case 0x2:
			raddr = inst->argliteral + OP(c);
			break;
		default:
			raddr = addrdecode(c, inst->argliteral);
			break;
		}
		if(memread32(c, raddr, &raddr))
			return -1;
		switch(inst->mode){
		case 0x1:
			break;
		case 0x2:
			raddr += OP(c);
			break;
		default:
			raddr = addrdecode(c, raddr);
			break;
		}
		goto stocommon;
		break;
	case MOV:
		regdata[0] = regdata[1];
		break;
	case SWAP:
		writescratch = 0;
		scratch = regdata[0];
		regdata[0] = regdata[1];
		regdata[1] = scratch;
		break;
	case PUSH:
		newsp = SP(c);
		newsp += 4;
		raddr = newsp;
		writesp = 1;
		scratch = regdata[0];
		writescratch = 4;
		break;
	// pusha and popa are effectively no-ops for now
	case PUSHA:
		if(memcheck(c, SP(c), 10*4))
			return ErrMem;
		dopusha = 1;
		break;
	case POPA:
		if(memcheck(c, SP(c), 10*4))
			return ErrMem;
		dopopa = 1;
		break;
	case POP:
		newsp = SP(c);
		raddr = newsp;
		newsp -= 4;
		writesp = 1;
		if(memread32(c, raddr, &scratch))
			return ErrMem;
		regdata[0] = scratch;
		break;
	case POPC:
		newsp = SP(c);
		newsp -= inst->argliteral;
		writesp = 1;
		break;
	case BSET:
		regdata[0] |= 1<<regdata[1];
		break;
	case BSETC:
		regdata[0] |= 1<<inst->argliteral;
		break;
	case BCLR:
		regdata[0] &= ~(1<<regdata[1]);
		break;
	case BCLRC:
		regdata[0] &= ~(1<<inst->argliteral);
		break;
	case BTEST:
		if(regdata[0] & (1<<regdata[1]))
			setcond = 1;
		break;
	case BTESTC:
		if(regdata[0] & (1<<inst->argliteral))
			setcond = 1;
		break;
	case ADD:
		regdata[0] = regdata[0] + regdata[1];
		break;
	case SUB:
		regdata[0] = regdata[0] - regdata[1];
		break;
	case MUL:
		regdata[0] = regdata[0] * regdata[1];
		break;
	case DIV:
		regdata[0] = regdata[0] / regdata[1];
		break;
	case MOD:
		regdata[0] = regdata[0] % regdata[1];
		break;
	case INC:
		regdata[0]++;
		break;
	case DEC:
		regdata[0]--;
		break;
	case SR:
		regdata[0] = regdata[0]<<regdata[1];
		break;
	case SL:
		regdata[0] = regdata[0]>>regdata[1];
		break;
	case NOT:
		regdata[0] = ~regdata[0];
		break;
	case AND:
		regdata[0] = regdata[0] & regdata[1];
		break;
	case OR:
		regdata[0] = regdata[0] | regdata[1];
		break;
	case XOR:
		regdata[0] = regdata[0] ^ regdata[1];
		break;
	case EQ:
		if(regdata[0] == regdata[1])
			setcond = 1;
		else
			unsetcond = 1;
		break;
	case NEQ:
		if(regdata[0] != regdata[1])
			setcond = 1;
		else
			unsetcond = 1;
		break;
	case LT:
		if(regdata[0] < regdata[1])
			setcond = 1;
		else
			unsetcond = 1;
		break;
	case GT:
		if(regdata[0] > regdata[1])
			setcond = 1;
		else
			unsetcond = 1;
		break;
	case NZ:
		if(regdata[0] != 0)
			setcond = 1;
		else
			unsetcond = 1;
		break;
	case JMP:
jmpcommon:
		switch(inst->mode){
		case 0x1:
			newpc = inst->argliteral;
			break;
		case 0x2:
			newpc = inst->argliteral + OP(c);
			break;
		default:
			newpc = addrdecode(c, inst->argliteral);
			break;
		}
		writepc = 1;
		break;
	case JMPR:
jmprcommon:
		switch(inst->mode){
		case 0x1:
			newpc = regdata[0];
			break;
		case 0x2:
			newpc = regdata[0] + OP(c);
			break;
		default:
			newpc = addrdecode(c, regdata[0]);
			break;
		}
		writepc = 1;
		break;
	case JZ:
		if(regdata[0] == 0)
			goto jmpcommon;
		break;
	case JZR:
		if(regdata[0] == 0){
			inst->argliteral = regdata[1];
			goto jmpcommon;
		}
		break;
	case JNZ:
		if(regdata[0] != 0)
			goto jmpcommon;
		break;
	case JNZR:
		if(regdata[0] != 0){
			inst->argliteral = regdata[1];
			goto jmpcommon;
		}
		break;
	case BR:
		if(FL(c) & 1<<3)
			goto jmpcommon;
		break;
	case BRR:
		if(FL(c) & 1<<3)
			goto jmprcommon;
		break;
	case CALL:
		newsp = SP(c);
		newsp += 4;
		raddr = newsp;
		scratch = c->nextpc;
		newpc = inst->argliteral;
		writepc = 1;
		writescratch = 4;
		break;
	case CALLR:
		newsp = SP(c);
		newsp += 4;
		raddr = newsp;
		scratch = c->nextpc;
		newpc = regdata[0];
		writepc = 1;
		writescratch = 4;
		break;
	case RET:
		if(memread32(c, SP(c), &newpc))
			return ErrMem;
		newsp = SP(c);
		newsp -= 4;
		writepc = 1;
		break;
	case SYSCALL:
		if(memcheck(c, SP(c), 2*4))
			return ErrMem;
		dosyscall = 1;
		break;
	case SYSRET:
		dosysret = 1;
		break;
	case IRET:
		doiret = 1;
		break;
	case INT:
		doint = 1;
		break;
	case NOP:
		// no-op
		break;
	case SINT:
		dosint = 1;
		break;
	case CINT:
		docint = 1;
		break;
	case HALT:
		c->halted = 1;
		break;
	case IN:
		switch(inst->width){
		case 0x1:
			regdata[0] = devread8(c, inst->argliteral);
			break;
		case 0x2:
			regdata[0] = devread16(c, inst->argliteral);
			break;
		case 0x3:
		default:
			regdata[0] = devread32(c, inst->argliteral);
			break;
		}
		break;
	case INR:
		switch(inst->width){
		case 0x1:
			regdata[0] = devread8(c, (u8int)regdata[1]);
			break;
		case 0x2:
			regdata[0] = devread16(c, (u16int)regdata[1]);
			break;
		case 0x3:
		default:
			regdata[0] = devread32(c, regdata[1]);
			break;
		}
		break;
	case OUT:
		switch(inst->width){
		case 0x1:
			devwrite8(c, inst->argliteral, (u8int)regdata[0]);
			break;
		case 0x2:
			devwrite16(c, inst->argliteral, (u16int)regdata[0]);
			break;
		case 0x3:
		default:
			devwrite32(c, inst->argliteral, regdata[0]);
			break;
		}
		break;
	case OUTR:
		switch(inst->width){
		case 0x1:
			devwrite8(c, regdata[0], (u8int)regdata[1]);
			break;
		case 0x2:
			devwrite16(c, regdata[0], (u16int)regdata[1]);
			break;
		case 0x3:
		default:
			devwrite32(c, regdata[0], regdata[1]);
			break;
		}
		break;
	case DINT:
		devint(c, inst->argliteral);
		break;
	case DINTR:
		devint(c, regdata[0]);
		break;
	default:
		return ErrInvalidInst;
		break;
	}

	// processor state updating
	switch(writescratch){
	case 1:
		if(memwrite(c, raddr, scratch8))
			return ErrMem;
		break;
	case 2:
		if(memwrite16(c, raddr, scratch16))
			return ErrMem;
		break;
	case 4:
		if(memwrite32(c, raddr, scratch32))
			return ErrMem;
		break;
	}
	if(writepc)
		c->nextpc = newpc;
	if(writesp)
		c->regs[regSP] = newsp;
	if(setcond)
		c->regs[regFL] |= 1<<3;
	if(unsetcond)
		c->regs[regFL] &= ~(1<<3);
	if(!(dopusha || dopopa))
		regwrite(c, inst, &regdata[0]);
	// special logic to do pusha and popa
	if(dopusha){
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR0]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR1]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR2]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR3]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR4]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR5]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR6]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regR7]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regOP]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regFL]);
	}
	if(dopopa){
		memread32(c, c->regs[regSP], &(c->regs[regFL]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regOP]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR7]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR6]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR5]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR4]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR3]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR2]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR1]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->regs[regR0]));
		c->regs[regSP] -= 4;
	}
	if(dosyscall){
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regPC]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regFL]);
		c->nextpc = c->regs[regSV];
		c->regs[regFL] &= ~(1<<0);
		c->regs[regFL] &= ~(1<<1);
	}
	if(dosysret || doiret){
		memread32(c, c->regs[regSP], &(c->regs[regFL]));
		c->regs[regSP] -= 4;
		memread32(c, c->regs[regSP], &(c->newpc));
		c->regs[regSP] -= 4;
	}
	if(doint){
		if(!(c->regs[regFL] & 1<<2))
			return 0;
		scratch8 = inst->argliteral;
		scratch8 &= 0xf;
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regPC]);
		c->regs[regSP] += 4;
		memwrite32(c, c->regs[regSP], c->regs[regFL]);
		c->nextpc = c->regs[regFV];
		c->regs[regFL] &= ~(1<<0);
		c->regs[regFL] &= ~(1<<1);
		c->regs[regFL] |= scratch8<<4;
	}
	if(dosint)
		c->regs[regFL] |= 1<<2;
	if(docint);
		c->regs[regFL] &= ~(1<<2);
	return 0;
}

