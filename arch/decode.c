#include <u.h>
#include <cpu.h>

int
fetch(Cpu *c, RawInst *rinst)
{
	u8int buf;
	u32int i;

	c->nextpc = PC(c);
	if(memread(c, c->nextpc++, &buf))
		return -1;
	rinst->len = buf & 0xf;
	rinst->mods = (buf & 0xf) >> 4;
	if(memread(c, c->nextpc++, &rinst->opcode))
		return -1;
	for(i = 0; i < rinst->len; i++){
		if(memread(c, c->nextpc++, &rinst->args[i]))
			return -1;
	}
}

Instdef*
findinstr(u8int opcode)
{
	Instdef *cur;
	int i;

	for(i = 0; instdefs[i].opcode != 0; i++)
		if(instdefs[i].opcode == opcode)
			return &instdefs[i];
	return nil;
}

Instdef*
findinstr_fast(u8int opcode)
{
	u8int off = opcode - 0x80;
	Instdef *cur;

	cur = &instdefs[off];
	if(cur->opcode != opcode)
		return findinstr(opcode);
	return cur;
}

int
decode(Cpu *c, RawInst *rinst, Inst *inst)
{
	Instdef *id;

	id = findinstr(rinst->opcode);
	if(id == nil)
		return -1;	// invalid instruction fault
	if(id->len != rinst->len)
		return -2;	// malformed instruction fault (length)

	inst->len = rinst->len;
	inst->opcode = rinst->opcode;
	inst->memwrite = 0;
	inst->usereg = 0;
	inst->condition = 0;
	if(rinst->mods & 0xf == 0xf)
		inst->condition = 1;
	else {
		if(id->mod1)
			inst->width = rinst->mods & 0x3;
		if(id->mod2)
			inst->mode = rinst->mods & (0x3 << 2);
	}

	if(id->regargsn == 2){
		// opcode reg1, reg2
		inst->usereg = 2;
		inst->argregs[0] = rinst->args[0] >> 4;
		inst->argregs[1] = rinst->args[0] & 0xf;
		return 0;
	}
	if(id->order && id->constargsz == 1 && id->regargsn == 1){
		// opcode u32int, reg
		inst->usereg = 1;
		inst->argliteral = (rinst->args[0] << 24) | (rinst->args[1] << 16) |
			(rinst->args[2] << 8) | rinst->args[3];
		inst->argregs[0] = args[4] >> 4;
		return 0;
	}
	if(id->constargsz == 1 && id->regargsn == 1) {
		// opcode reg, u8int
		inst->usereg = 1;
		inst->argregs[0] = rinst->args[0] >> 4;
		inst->argliteral = rinst->args[1];
		return 0;
	}
	if(id->constargsz == 4 && id->regargsn == 1) {
		// opcode reg, u32int
		inst->usereg = 1;
		inst->argregs[0] = rinst->args[0] >> 4;
		inst->argliteral = (rinst->args[1] << 24) | (rinst->args[2] << 16) |
			(rinst->args[3] << 8) | rinst->args[4];
		return 0;
	}
	if(id->constargsz == 4 && id->regargsn == 0) {
		// opcode u32int
		inst->argliteral = (rinst->args[0] << 24) | (rinst->args[1] << 16) |
			(rinst->args[2] << 8) | rinst->args[3];
		return 0;
	}
	if(id->constargsz == 1 && id->regargsn == 0){
		// opcode u8int
		inst->argliteral = rinst->args[0];
		return 0;
	}
	if(id->regargsn == 1) {
		// opcode reg
		inst->usereg = 1;
		inst->argregs[0] = rinst->args[0] >> 4;
		return 0;
	}
	if(id->regargsn == 0 && constargsz == 0) {
		// opcode
		return 0;
	}
	return -3;	// malformed instruction fault (bad inst def in vm)
}

