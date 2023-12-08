#ifndef CPU_H
#define CPU_H

#include "memory.h"

#include <cstdint>
#include <array>

class CPU {
public:
    CPU();
    void initialize();
    void executeInstruction(uint8_t opcode);
    void reset();

private:
    void initializeLookupTable();
    void unimplementedInstruction();
    void updateNegativeFlag(uint8_t value);
    void updateOverflowFlag(uint8_t value);
    void updateZeroFlag(uint8_t value);
    void updateCarryFlag(uint8_t value);
    void setFlag(uint8_t flag, bool value);
    static bool checkPageCrossing(uint16_t oldAddress, uint16_t newAddress);

    void runOneCycle();

    // Opcodes
    void adcImmediate();
    void adcZpg();
    void adcZpgX();
    void adcAbsolute();
    void adcAbsoluteX();
    void adcAbsoluteY();
    void adcIndirectX();
    void adcIndirectY();

    void andImmediate();
    void andZpg();
    void andZpgX();
    void andAbsolute();
    void andAbsoluteX();
    void andAbsoluteY();
    void andIndirectX();
    void andIndirectY();

    void aslA();
    void aslZpg();
    void aslZpgX();
    void aslAbsolute();
    void aslAbsoluteX();

    void bcc();

    void bcs();

    void beq();

    void bitZpg();
    void bitAbsolute();

    void bmi();

    void bne();

    void bpl();

    void brk();

    void bvc();

    void bvs();

    void clc();

    void cld();

    void cli();

    void clv();

    void cmpImmediate();
    void cmpZpg();
    void cmpZpgX();
    void cmpAbsolute();
    void cmpAbsoluteX();
    void cmpAbsoluteY();
    void cmpIndirectX();
    void cmpIndirectY();

    void cpxImmediate();
    void cpxZpg();
    void cpxAbsolute();

    void cpyImmediate();
    void cpyZpg();
    void cpyAbsolute();

    void decZpg();
    void decZpgX();
    void decAbsolute();
    void decAbsoluteX();

    void dex();

    void dey();

    void eorImmediate();
    void eorZpg();
    void eorZpgX();
    void eorAbsolute();
    void eorAbsoluteX();
    void eorAbsoluteY();
    void eorIndirectX();
    void eorIndirectY();

    void incZpg();
    void incZpgX();
    void incAbsolute();
    void incAbsoluteX();

    void inx();

    void iny();

    void jmpAbsolute();
    void jmpIndirect();

    void jsr();

    void ldaImmediate();
    void ldaZpg();
    void ldaZpgX();
    void ldaAbsolute();
    void ldaAbsoluteX();
    void ldaAbsoluteY();
    void ldaIndirectX();
    void ldaIndirectY();

    void ldxImmediate();
    void ldxZpg();
    void ldxZpgY();
    void ldxAbsolute();
    void ldxAbsoluteY();

    void ldyImmediate();
    void ldyZpg();
    void ldyZpgX();
    void ldyAbsolute();
    void ldyAbsoluteX();

    void lsrA();
    void lsrZpg();
    void lsrZpgX();
    void lsrAbsolute();
    void lsrAbsoluteX();

    void nop();

    void oraImmediate();
    void oraZpg();
    void oraZpgX();
    void oraAbsolute();
    void oraAbsoluteX();
    void oraAbsoluteY();
    void oraIndirectX();
    void oraIndirectY();

    void pha();

    void php();

    void pla();

    void plp();

    void rolA();
    void rolZpg();
    void rolZpgX();
    void rolAbsolute();
    void rolAbsoluteX();

    void rorA();
    void rorZpg();
    void rorZpgX();
    void rorAbsolute();
    void rorAbsoluteX();

    void rti();

    void rts();

    void sbcImmediate();
    void sbcZpg();
    void sbcZpgX();
    void sbcIndirectX();
    void sbcIndirectY();
    void sbcAbsolute();
    void sbcAbsoluteX();
    void sbcAbsoluteY();

    void sec();

    void sed();

    void sei();

    void staZpg();
    void staZpgX();
    void staAbsolute();
    void staAbsoluteX();
    void staAbsoluteY();
    void staIndirectX();
    void staIndirectY();

    void stxZpg();
    void stxZpgY();
    void stxAbsolute();

    void styZpg();
    void styZpgX();
    void styAbsolute();

    void tax();

    void tay();

    void tsx();

    void txa();

    void txs();

    void tya();

    // Addressing Modes
    // Structure to hold address and value info for addressing modes
    struct AddressValue {
        uint16_t effectiveAddress;
        uint16_t oldEffectiveAddress;
        uint8_t value;
    };
    uint8_t getImmediateAddress();
    AddressValue getZeroPageAddress();
    AddressValue getZeroPageXAddress();
    AddressValue getZeroPageYAddress();
    AddressValue getAbsoluteAddress();
    AddressValue getAbsoluteXAddress();
    AddressValue getAbsoluteYAddress();
    uint8_t getIndirectAddress();
    AddressValue getIndirectXAddress();
    AddressValue getIndirectYAddress();

public:
private:
    // Type alias for a pointer to a member function of CPU
    using InstructionFunc = void (CPU::*)();

    // Instruction Lookup Table
    std::array<InstructionFunc, 256> lookupTable;

    // Registers
    uint16_t PC;            // 16 bit Program Counter
    uint8_t Accumulator;    // 8 bit
    uint8_t X_Register;     // 8 bit
    uint8_t Y_Register;     // 8 bit
    uint8_t SP;             // 8 bit Stack Pointer
    uint8_t SR;             // 8 bit Processor Status Flags (Status Register / P Register)

    // Individual Flags
    static constexpr uint8_t CARRY_FLAG = 0X01;
    static constexpr uint8_t ZERO_FLAG = 0X02;
    static constexpr uint8_t INTERRUPT_FLAG = 0X04;
    static constexpr uint8_t DECIMAL_FLAG = 0x08;
    static constexpr uint8_t BREAK_FLAG = 0X10;
    static constexpr uint8_t OVERFLOW_FLAG = 0X40;
    static constexpr uint8_t NEGATIVE_FLAG = 0X80;

    // Memory
    Memory memoryModule;

    // Cycles
    int cycles; // Tracks the number of cycles
};

#endif // CPU_H