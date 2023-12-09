#include "tia.h"
#include <iostream>

TIA::TIA() : VSYNC(0), VBLANK(0) {
    //TODO
    // initialize other registers
}

// TIA memory-mapped write-only registers
void TIA::write(uint16_t address, uint8_t value) {
    switch (address) {
        case 0x00: VSYNC = value; break;
        case 0x01: VBLANK = value; break;
        case 0x02: WSYNC = value; break;
        case 0x03: RSYNC = value; break;
        case 0x04: NUSIZ0 = value; break;
        case 0x05: NUSIZ1 = value; break;
        case 0x06: COLUP0 = value; break;
        case 0x07: COLUP1 = value; break;
        case 0x08: COLUPF = value; break;
        case 0x09: COLUBK = value; break;
        case 0x0A: CTRLPF = value; break;
        case 0x0B: REFP0 = value; break;
        case 0x0C: REFP1 = value; break;
        case 0x0D: PF0 = value; break;
        case 0x0E: PF1 = value; break;
        case 0x0F: PF2 = value; break;
        case 0x10: RESP0 = value; break;
        case 0x11: RESP1 = value; break;
        case 0x12: RESM0 = value; break;
        case 0x13: RESM1 = value; break;
        case 0x14: RESBL = value; break;
        case 0x15: AUDC0 = value; break;
        case 0x16: AUDC1 = value; break;
        case 0x17: AUDF0 = value; break;
        case 0x18: AUDF1 = value; break;
        case 0x19: AUDV0 = value; break;
        case 0x1A: AUDV1 = value; break;
        case 0x1B: GRP0 = value; break;
        case 0x1C: GRP1 = value; break;
        case 0x1D: ENAM0 = value; break;
        case 0x1E: ENAM1 = value; break;
        case 0x1F: ENABL = value; break;
        case 0x20: HMP0 = value; break;
        case 0x21: HMP1 = value; break;
        case 0x22: HMM0 = value; break;
        case 0x23: HMM1 = value; break;
        case 0x24: HMBL = value; break;
        case 0x25: VDELP0 = value; break;
        case 0x26: VDELP1 = value; break;
        case 0x27: VDELBL = value; break;
        case 0x28: RESMP0 = value; break;
        case 0x29: RESMP1 = value; break;
        case 0x2A: HMOVE = value; break;
        case 0x2B: HMCLR = value; break;
        case 0x2C: CXCLR = value; break;
        default: std::cerr << "unimplemented write address" << std::endl;
    }
}

// TIA memory-mapped read-only registers
uint8_t TIA::read(uint16_t address) {
    switch (address) {
        case 0x00: return CXM0P;
        case 0x01: return CXM1P;
        case 0x02: return CXP0FB;
        case 0x03: return CXP1FB;
        case 0x04: return CXM0FB;
        case 0x05: return CXM1FB;
        case 0x06: return CXBLPF;
        case 0x07: return CXPPMM;
        case 0x08: return INPT0;
        case 0x09: return INPT1;
        case 0x0A: return INPT2;
        case 0x0B: return INPT3;
        case 0x0C: return INPT4;
        case 0x0D: return INPT5;
        default: std::cerr << "unimplemented read address" << std::endl;
    }
}