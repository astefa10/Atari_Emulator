#ifndef ATARI_EMULATOR_TIA_H
#define ATARI_EMULATOR_TIA_H
#include <cstdint>

class TIA {
public:
    TIA();
    void write(uint16_t address, uint8_t value);
    uint8_t read(uint16_t address);

private:
    // write addresses
    uint8_t VSYNC;
    uint8_t VBLANK;
    uint8_t WSYNC;
    uint8_t RSYNC;
    uint8_t NUSIZ0;
    uint8_t NUSIZ1;
    uint8_t COLUP0;
    uint8_t COLUP1;
    uint8_t COLUPF;
    uint8_t COLUBK;
    uint8_t CTRLPF;
    uint8_t REFP0;
    uint8_t REFP1;
    uint8_t PF0;
    uint8_t PF1;
    uint8_t PF2;
    uint8_t RESP0;
    uint8_t RESP1;
    uint8_t RESM0;
    uint8_t RESM1;
    uint8_t RESBL;
    uint8_t AUDC0;
    uint8_t AUDC1;
    uint8_t AUDF0;
    uint8_t AUDF1;
    uint8_t AUDV0;
    uint8_t AUDV1;
    uint8_t GRP0;
    uint8_t GRP1;
    uint8_t ENAM0;
    uint8_t ENAM1;
    uint8_t ENABL;
    uint8_t HMP0;
    uint8_t HMP1;
    uint8_t HMM0;
    uint8_t HMM1;
    uint8_t HMBL;
    uint8_t VDELP0;
    uint8_t VDELP1;
    uint8_t VDELBL;
    uint8_t RESMP0;
    uint8_t RESMP1;
    uint8_t HMOVE;
    uint8_t HMCLR;
    uint8_t CXCLR;

    // read addresses
    uint8_t CXM0P;
    uint8_t CXM1P;
    uint8_t CXP0FB;
    uint8_t CXP1FB;
    uint8_t CXM0FB;
    uint8_t CXM1FB;
    uint8_t CXBLPF;
    uint8_t CXPPMM;
    uint8_t INPT0;
    uint8_t INPT1;
    uint8_t INPT2;
    uint8_t INPT3;
    uint8_t INPT4;
    uint8_t INPT5;

};


#endif //ATARI_EMULATOR_TIA_H
