#include "cpu.h"
#include <iostream>
#include <bit>

// Constructor
CPU::CPU() : SP(0xFF), cycles(0) {
    initializeLookupTable();
};

// Opcode Functions ----------------------------------------------------------------------------------------------------

// Add Memory to Accumulator with Carry
void CPU::adcImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    uint16_t temp = Accumulator + value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    cycles += 2;
}
void CPU::adcZpg() {
    AddressValue addressData = getZeroPageAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    cycles += 3;
}
void CPU::adcZpgX() {
    AddressValue addressData = getZeroPageYAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    cycles += 4;
}
void CPU::adcAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    cycles += 4;
}
void CPU::adcAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }

    cycles += 4;
}
void CPU::adcAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }

    cycles += 4;
}
void CPU::adcIndirectX() {
    AddressValue addressData = getIndirectXAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    cycles += 6;
}
void CPU::adcIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    uint16_t temp = Accumulator + addressData.value + (SR & CARRY_FLAG);

    setFlag(CARRY_FLAG, temp > 0xFF);
    updateNegativeFlag(temp & 0xFF);
    updateZeroFlag(temp & 0xFF);

    bool overflow = ((Accumulator ^ addressData.value) & 0x80) == 0 && ((Accumulator ^ temp) & 0x80) != 0;
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = temp & 0xFF;

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }

    cycles += 5;
}

// AND with Accumulator
void CPU::andImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    Accumulator &= value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::andZpg() {
    AddressValue addressData = getZeroPageAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 3;
}
void CPU::andZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::andAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::andAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::andAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::andIndirectX() {
    AddressValue addressData = getIndirectXAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 6;
}
void CPU::andIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    Accumulator &= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    // Check for page crossing
    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) { // Page crossed
        cycles += 1;
    }
    cycles += 5;
}

// Arithmetic shift left
void CPU::aslA() {
    PC++;

    // Set CARRY_FLAG to the 7th bit value before shifting
    setFlag(CARRY_FLAG, Accumulator & 0x80);
    Accumulator <<= 1;
    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::aslZpg() {
    AddressValue addressData = getZeroPageAddress();

    // Set CARRY_FLAG to the 7th bit value before shifting
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value <<= 1;

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 5;
}
void CPU::aslZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    // Set CARRY_FLAG to the 7th bit value before shifting
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value <<= 1;

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::aslAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    // Set CARRY_FLAG to the 7th bit value before shifting
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value <<= 1;

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::aslAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    // Set CARRY_FLAG to the 7th bit value before shifting
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value <<= 1;

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress,addressData.value);

    cycles += 7;
}

// Branch on Carry Clear
void CPU::bcc() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    if (!(SR & CARRY_FLAG)) {
        uint16_t oldPC = PC;
        PC += offset;

        if (checkPageCrossing(oldPC, PC)) {
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Branch on Carry Set
void CPU::bcs() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    if (SR & CARRY_FLAG) {
        uint16_t oldPC = PC;
        PC += offset;

        if (checkPageCrossing(oldPC, PC)) {
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Branch on Result Zero
void CPU::beq() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    if (SR & ZERO_FLAG) {
        uint16_t oldPC = PC;
        PC += offset;

        if (checkPageCrossing(oldPC, PC)) {
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Test Bits in Memory with Accumulator
void CPU::bitZpg() {
    PC++;
    uint8_t zeroPageAddress = memoryModule.readMemory(PC++);
    uint16_t effectiveAddress = zeroPageAddress;

    uint8_t value = memoryModule.readMemory(effectiveAddress);

    updateNegativeFlag(value);
    updateOverflowFlag(value);

    updateZeroFlag(Accumulator & value);

    cycles += 3;
}
void CPU::bitAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    updateNegativeFlag(addressData.value);
    updateOverflowFlag(addressData.value);

    updateZeroFlag(Accumulator & addressData.value);

    cycles += 4;
}

// Branch on minus
void CPU::bmi() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    // Check if NEGATIVE_FLAG is set
    if (SR & NEGATIVE_FLAG) {
        uint16_t oldPC = PC;
        PC += offset;

        // Check for page crossing
        if (checkPageCrossing(oldPC, PC)) { // Page boundary crossed
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Branch on Result not Zero
void CPU::bne() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    if (!(SR & ZERO_FLAG)) {
        uint16_t oldPC = PC;
        PC += offset;

        if (checkPageCrossing(oldPC, PC)) {
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Branch on plus
void CPU::bpl() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    // Check if NEGATIVE_FLAG is not set
    if (!(SR & NEGATIVE_FLAG)) {
        uint16_t oldPC = PC;
        PC += offset;

        // Check for page crossing
        if (checkPageCrossing(oldPC, PC)) { // Page boundary crossed
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Break
void CPU::brk() {
     // Account for extra byte of spacing
     // for a break mark
     PC += 2;

     // Little Endian, push high byte first
     memoryModule.pushStack(SP,(PC >> 8) & 0xFF);   // Push high byte of return address
     memoryModule.pushStack(SP,PC & 0xFF);          // Push low byte of return address

     uint8_t statusWithBreakFlag = SR | BREAK_FLAG; // Set break flag
     memoryModule.pushStack(SP,statusWithBreakFlag);          // Push status new

     uint16_t irqAddress = (memoryModule.readMemory(0xFFFF) << 8) | memoryModule.readMemory(0xFFFE);  // Read the IRQ (Interrupt Request) vector
     PC = irqAddress;   // Set the PC to the IRQ vector address

     cycles += 7;
 }

// Branch on Overflow Clear
void CPU::bvc() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    // Check if OVERFLOW_FLAG is not set
    if (!(SR & OVERFLOW_FLAG)) {
        uint16_t oldPC = PC;
        PC += offset;

        // Check for page crossing
        if (checkPageCrossing(oldPC, PC)) {
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Branch on Overflow Set
void CPU::bvs() {
    PC++;
    auto offset = static_cast<int8_t>(memoryModule.readMemory(PC++));

    // Check if OVERFLOW_FLAG is set
    if (SR & OVERFLOW_FLAG) {
        uint16_t oldPC = PC;
        PC += offset;

        // Check for page crossing
        if (checkPageCrossing(oldPC, PC)) {
            cycles += 2;
        } else {
            cycles += 1;
        }
    }
    cycles += 2;
}

// Clear Carry Flag
void CPU::clc() {
    PC++;
    setFlag(CARRY_FLAG, false);
    cycles += 2;
}

// Clear Decimal Mode
void CPU::cld() {
    PC++;
    setFlag(DECIMAL_FLAG, false);
    cycles += 2;
}

// Clear Interrupt Disable Bit
void CPU::cli() {
    PC++;
    setFlag(INTERRUPT_FLAG, false);
    cycles += 2;
}

// Clear Overflow Flag
void CPU::clv() {
    PC++;
    setFlag(OVERFLOW_FLAG, false);
    cycles += 2;
}

// Compare Memory with Accumulator
void CPU::cmpImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(value);

    setFlag(CARRY_FLAG, Accumulator >= value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 2;
}
void CPU::cmpZpg() {
    AddressValue addressData = getZeroPageAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 3;
}
void CPU::cmpZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 4;
}
void CPU::cmpAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 4;
}
void CPU::cmpAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::cmpAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::cmpIndirectX() {
    AddressValue addressData = getIndirectXAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 6;
}
void CPU::cmpIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    uint16_t result = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Accumulator >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 5;
}

// Compare Memory and Index X
void CPU::cpxImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    uint16_t result = static_cast<uint16_t>(X_Register) - static_cast<uint16_t>(value);

    setFlag(CARRY_FLAG, X_Register >= value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 2;
}
void CPU::cpxZpg() {
    AddressValue addressData = getZeroPageAddress();

    uint16_t result = static_cast<uint16_t>(X_Register) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, X_Register >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 3;
}
void CPU::cpxAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    uint16_t result = static_cast<uint16_t>(X_Register) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, X_Register >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 4;
}

// Compare Memory and Index Y
void CPU::cpyImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    uint16_t result = static_cast<uint16_t>(Y_Register) - static_cast<uint16_t>(value);

    setFlag(CARRY_FLAG, Y_Register >= value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 2;
}
void CPU::cpyZpg() {
    AddressValue addressData = getZeroPageAddress();

    uint16_t result = static_cast<uint16_t>(Y_Register) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Y_Register >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 3;
}
void CPU::cpyAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    uint16_t result = static_cast<uint16_t>(Y_Register) - static_cast<uint16_t>(addressData.value);

    setFlag(CARRY_FLAG, Y_Register >= addressData.value);
    updateZeroFlag(result & 0xFF);
    updateNegativeFlag(result & 0xFF);

    cycles += 4;
}

// Decrement Memory by One
void CPU::decZpg() {
    AddressValue addressData = getZeroPageAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, --addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 5;
}
void CPU::decZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, --addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 6;
}
void CPU::decAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, --addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 6;
}
void CPU::decAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, --addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 7;
}

// Decrement Index X by One
void CPU::dex() {
    PC++;

    X_Register--;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 2;
}

// Decrement Index Y by One
void CPU::dey() {
    PC++;

    Y_Register--;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 2;
}

// Exclusive-OR Memory with Accumulator
void CPU::eorImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    Accumulator ^= value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::eorZpg() {
    AddressValue addressData = getZeroPageAddress();

    Accumulator ^= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 3;
}
void CPU::eorZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    Accumulator ^= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::eorAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    Accumulator ^= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::eorAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    Accumulator ^= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::eorAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    Accumulator ^= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::eorIndirectX() {
    AddressValue addressValue = getIndirectXAddress();

    Accumulator ^= addressValue.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 6;
}
void CPU::eorIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    Accumulator ^= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 5;
}

// Increment Memory by One
void CPU::incZpg() {
    AddressValue addressData = getZeroPageAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, ++addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 5;
}
void CPU::incZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, ++addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 6;
}
void CPU::incAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, ++addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 6;
}
void CPU::incAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, ++addressData.value);

    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    cycles += 7;
}

// Increment Index X by One
void CPU::inx() {
    PC++;

    X_Register++;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 2;
}

// Increment Index Y by One
void CPU::iny() {
    PC++;

    Y_Register++;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 2;
}

// Jump to New Location
void CPU::jmpAbsolute() {
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(PC + 1);
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(PC + 2);

    PC = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;

    cycles += 3;
}
void CPU::jmpIndirect() {
    uint8_t indirectAddressLowByte = memoryModule.readMemory(PC + 1);
    uint8_t indirectAddressHighByte = memoryModule.readMemory(PC + 2);
    uint16_t indirectAddress = (indirectAddressHighByte << 8) | indirectAddressLowByte;

    // Handle the 6502 page boundary bug
    uint16_t effectiveAddressLowByte = memoryModule.readMemory(indirectAddress);
    uint16_t effectiveAddressHighByte = memoryModule.readMemory((indirectAddress & 0xFF00) | ((indirectAddress + 1) & 0x00FF));

    /* Don't handle the bug
    uint16_t effectiveAddressLowByte = memoryModule.readMemory(indirectAddress);
    uint16_t effectiveAddressHighByte = memoryModule.readMemory(indirectAddress + 1);
    */

    PC = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;

    cycles += 5;
}

// Jump to New Location Saving Return Address
void CPU::jsr() {
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(PC + 1);
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(PC + 2);

    // Push the return address onto the stack
    uint16_t returnAddress = PC + 2;
    memoryModule.pushStack(SP, (returnAddress >> 8) & 0xFF);
    memoryModule.pushStack(SP, returnAddress & 0xFF);

    // Set PC to the effective address
    PC = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;

    cycles += 6;
}

// Load Accumulator with Memory
void CPU::ldaImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    Accumulator = value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::ldaZpg() {
    AddressValue addressData = getZeroPageAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 3;
}
void CPU::ldaZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::ldaAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::ldaAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::ldaAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::ldaIndirectX() {
    AddressValue addressData = getIndirectXAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 6;
}
void CPU::ldaIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    Accumulator = addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 5;
}

// Load Index X with Memory
void CPU::ldxImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    X_Register = value;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 2;
}
void CPU::ldxZpg() {
    AddressValue addressData = getZeroPageAddress();

    X_Register = addressData.value;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 3;
}
void CPU::ldxZpgY() {
    AddressValue addressData = getZeroPageYAddress();

    X_Register = addressData.value;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 4;
}
void CPU::ldxAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    X_Register = addressData.value;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 4;
}
void CPU::ldxAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    X_Register = addressData.value;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}

// Load Index Y with Memory
void CPU::ldyImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    Y_Register = value;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 2;
}
void CPU::ldyZpg() {
    AddressValue addressData = getZeroPageAddress();

    Y_Register = addressData.value;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 3;
}
void CPU::ldyZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    Y_Register = addressData.value;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 4;
}
void CPU::ldyAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    Y_Register = addressData.value;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 4;
}
void CPU::ldyAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    Y_Register = addressData.value;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}

// Shift One Bit Right
void CPU::lsrA() {
    PC++;

    setFlag(CARRY_FLAG, Accumulator & 0x01);    // Set CARRY_FLAG to the 0th bit value before shifting

    Accumulator >>= 1;
    setFlag(NEGATIVE_FLAG, false);  // NEGATIVE_FLAG is cleared
    updateZeroFlag(Accumulator);    // Update ZERO_FLAG based on new value

    cycles += 2;
}
void CPU::lsrZpg() {
    PC++;
    uint8_t zeroPageAddress = memoryModule.readMemory(PC++);
    uint16_t effectiveAddress = zeroPageAddress;

    uint8_t value = memoryModule.readMemory(effectiveAddress);
    setFlag(CARRY_FLAG, value & 0x01);  // Set CARRY_FLAG to the 0th bit value before shifting

    value >>= 1;
    setFlag(NEGATIVE_FLAG, false);  // NEGATIVE_FLAG is cleared
    updateZeroFlag(value);  // Update ZERO_FLAG based on new value

    memoryModule.writeMemory(effectiveAddress, value);

    cycles += 5;
}
void CPU::lsrZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01);

    addressData.value >>= 1;
    setFlag(NEGATIVE_FLAG, false);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::lsrAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01); // Set CARRY_FLAG to the 0th bit value before shifting

    addressData.value >>= 1;
    setFlag(NEGATIVE_FLAG, false);  // NEGATIVE_FLAG is cleared
    updateZeroFlag(addressData.value);  // Update ZERO_FLAG based on value

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::lsrAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01);

    addressData.value >>= 1;
    setFlag(NEGATIVE_FLAG, false);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 7;
}

// No Operation
void CPU::nop() {
    PC++;

    cycles += 2;
}

// OR with Accumulator
void CPU::oraImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    Accumulator |= value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::oraZpg() {
    AddressValue addressData = getZeroPageAddress();

    Accumulator |= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 3;
}
void CPU::oraZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    Accumulator |= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::oraAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    Accumulator |= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}
void CPU::oraAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    Accumulator |= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::oraAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    Accumulator |= addressData.value;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::oraIndirectX() {
     AddressValue addressData = getIndirectXAddress();

     Accumulator |= addressData.value; // Perform bitwise OR with Accumulator

     updateNegativeFlag(Accumulator);
     updateZeroFlag(Accumulator);

     cycles += 6;
 }
void CPU::oraIndirectY() {
     AddressValue addressData = getIndirectYAddress();

     Accumulator |= addressData.value;

     updateNegativeFlag(Accumulator);
     updateZeroFlag(Accumulator);

     // Check for page crossing
     if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
         cycles += 1;    // Page crossed
     }

     cycles += 5;
}

// Push Accumulator on Stack
void CPU::pha() {
    PC++;

    memoryModule.pushStack(SP, Accumulator);

    cycles += 3;
}

// Push Processor Status on Stack
void CPU::php() {
     PC++;

     uint8_t statusWithBreakAnd5 = SR | BREAK_FLAG | 0x20; // Set break flag and 5th bit
     memoryModule.pushStack(SP, statusWithBreakAnd5);

     cycles += 3;
 }

// Pull accumulator from Stack
void CPU::pla() {
    PC++;
    Accumulator = memoryModule.popStack(SP);

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 4;
}

// Pull Processor Status from Stack
void CPU::plp() {
    PC++;
    uint8_t value = memoryModule.popStack(SP);

    SR = value & ~(BREAK_FLAG | 0x20);

    cycles += 4;
}

// Rotate left
void CPU::rolA() {
    PC++;

    // Set CARRY_FLAG to the 7th bit value before rotating
    setFlag(CARRY_FLAG, Accumulator & 0x80);
    Accumulator = std::rotl(Accumulator, 1);
    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::rolZpg() {
    AddressValue addressData = getZeroPageAddress();

    // Set CARRY_FLAG to the 7th bit value before rotating
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value = std::rotl(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 5;
}
void CPU::rolZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    // Set CARRY_FLAG to the 7th bit value before rotating
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value = std::rotl(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::rolAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    // Set CARRY_FLAG to the 7th bit value before rotating
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value = std::rotl(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::rolAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    // Set CARRY_FLAG to the 7th bit value before rotating
    setFlag(CARRY_FLAG, addressData.value & 0x80);

    addressData.value = std::rotl(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 7;
}

// Rotate one bit right
void CPU::rorA() {
    PC++;

    setFlag(CARRY_FLAG, Accumulator & 0x01);
    Accumulator = std::rotr(Accumulator, 1);
    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}
void CPU::rorZpg() {
    AddressValue addressData = getZeroPageAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01);

    addressData.value = std::rotr(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 5;
}
void CPU::rorZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01);

    addressData.value = std::rotr(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::rorAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01);

    addressData.value = std::rotr(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 6;
}
void CPU::rorAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    setFlag(CARRY_FLAG, addressData.value & 0x01);

    addressData.value = std::rotr(addressData.value, 1);
    updateNegativeFlag(addressData.value);
    updateZeroFlag(addressData.value);

    memoryModule.writeMemory(addressData.effectiveAddress, addressData.value);

    cycles += 7;
}

// Return from Interrupt
void CPU::rti() {
    uint8_t newSR = memoryModule.popStack(SP);
    SR = newSR & ~(BREAK_FLAG |  0x20);

    uint8_t pcLowByte = memoryModule.popStack(SP);
    uint8_t pcHighByte = memoryModule.popStack(SP);
    PC = (pcHighByte << 8) | pcLowByte;

    cycles += 6;
}

// Return from Subroutine
void CPU::rts() {
    uint8_t pcLowByte = memoryModule.popStack(SP);
    uint8_t pcHighByte = memoryModule.popStack(SP);
    PC = ((pcHighByte << 8) | pcLowByte) + 1;

    cycles += 6;
}

// Subtract Memory form Accumulator with Borrow
void CPU::sbcImmediate() {
    PC++;
    uint8_t value = memoryModule.readMemory(PC++);

    uint16_t temp = Accumulator - value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    cycles += 2;
}
void CPU::sbcZpg() {
    AddressValue addressData = getZeroPageAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    cycles += 3;
}
void CPU::sbcZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    cycles += 4;
}
void CPU::sbcAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    cycles += 4;
}
void CPU::sbcAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::sbcAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 4;
}
void CPU::sbcIndirectX() {
    AddressValue addressData = getIndirectXAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    cycles += 6;
}
void CPU::sbcIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    uint16_t temp = Accumulator - addressData.value - (!(SR & CARRY_FLAG));

    setFlag(CARRY_FLAG, temp > 0xFF);

    uint8_t result = temp & 0xFF;
    updateNegativeFlag(result);
    updateZeroFlag(result);

    bool overflow = (((Accumulator ^ result) & 0x80) != 0) && (((Accumulator ^ addressData.value) & 0x80) == 0);
    setFlag(OVERFLOW_FLAG, overflow);

    Accumulator = result;

    if (checkPageCrossing(addressData.oldEffectiveAddress, addressData.effectiveAddress)) {
        cycles += 1;
    }
    cycles += 5;
}

// Set Carry Flag
void CPU::sec() {
    PC++;
    setFlag(CARRY_FLAG, true);

    cycles += 2;
}

// Set Decimal Flag
void CPU::sed() {
    PC++;
    setFlag(DECIMAL_FLAG, true);

    cycles += 2;
}

// Set Interrupt Disable Status
void CPU::sei() {
    PC++;
    setFlag(INTERRUPT_FLAG, true);

    cycles += 2;
}

// Store Accumulator in Memory
void CPU::staZpg() {
    AddressValue addressData = getZeroPageAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 3;
}
void CPU::staZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 4;
}
void CPU::staAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 4;
}
void CPU::staAbsoluteX() {
    AddressValue addressData = getAbsoluteXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 5;
}
void CPU::staAbsoluteY() {
    AddressValue addressData = getAbsoluteYAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 5;
}
void CPU::staIndirectX() {
    AddressValue addressData = getIndirectXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 6;
}
void CPU::staIndirectY() {
    AddressValue addressData = getIndirectYAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Accumulator);

    cycles += 6;
}

// Store Index X in Memory
void CPU::stxZpg() {
    AddressValue addressData = getZeroPageAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, X_Register);

    cycles += 3;
}
void CPU::stxZpgY() {
    AddressValue addressData = getZeroPageYAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, X_Register);

    cycles += 4;
}
void CPU::stxAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, X_Register);

    cycles += 4;
}

// Store Index Y in Memory
void CPU::styZpg() {
    AddressValue addressData = getZeroPageAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Y_Register);

    cycles += 3;
}
void CPU::styZpgX() {
    AddressValue addressData = getZeroPageXAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Y_Register);

    cycles += 4;
}
void CPU::styAbsolute() {
    AddressValue addressData = getAbsoluteAddress();

    memoryModule.writeMemory(addressData.effectiveAddress, Y_Register);

    cycles += 4;
}

// Transfer Accumulator to Index X
void CPU::tax() {
    PC++;

    X_Register = Accumulator;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 2;
}

// Transfer Accumulator to Index Y
void CPU::tay() {
    PC++;

    Y_Register = Accumulator;

    updateNegativeFlag(Y_Register);
    updateZeroFlag(Y_Register);

    cycles += 2;
}

// Transfer Stack Pointer to Index X
void CPU::tsx() {
    PC++;

    X_Register = SP;

    updateNegativeFlag(X_Register);
    updateZeroFlag(X_Register);

    cycles += 2;
}

// Transfer Index X to Accumulator
void CPU::txa() {
    PC++;

    Accumulator = X_Register;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}

// Transfer Index X to Stack Register
void CPU::txs() {
    PC++;

    SP = X_Register;

    cycles += 2;
}

// Transfer Index Y to Accumulator
void CPU::tya() {
    PC++;

    Accumulator = Y_Register;

    updateNegativeFlag(Accumulator);
    updateZeroFlag(Accumulator);

    cycles += 2;
}

// Placeholder opcode
void CPU::unimplementedInstruction() {
    std::cerr << "Unimplemented opcode executed" << std::endl;
}

// Addressing Modes ----------------------------------------------------------------------------------------------------

uint8_t CPU::getImmediateAddress() {

}
CPU::AddressValue CPU::getZeroPageAddress() {
    PC++;
    uint8_t zeroPageAddress = memoryModule.readMemory(PC++); // Read 8-bit zero-page address
    uint16_t effectiveAddress = zeroPageAddress; // Convert to 16-bit with 0x00 as the high byte

    uint8_t value = memoryModule.readMemory(effectiveAddress);

    return {effectiveAddress, 0, value}; // Read value from address
}
CPU::AddressValue CPU::getZeroPageXAddress() {
    PC++;
    uint8_t zeroPageAddress = memoryModule.readMemory(PC++);
    uint16_t effectiveAddress = zeroPageAddress + X_Register;

    uint8_t value = memoryModule.readMemory(effectiveAddress);

    return {effectiveAddress, 0, value};
}
CPU::AddressValue CPU::getZeroPageYAddress() {

}
CPU::AddressValue CPU::getAbsoluteAddress() {
    PC++;
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(PC++);
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(PC++);
    uint16_t effectiveAddress = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;

    uint8_t value = memoryModule.readMemory(effectiveAddress);

    return {effectiveAddress, 0, value};
}
CPU::AddressValue CPU::getAbsoluteXAddress() {
    PC++;
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(PC++);
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(PC++);
    uint16_t oldEffectiveAddress = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;
    uint16_t newEffectiveAddress = oldEffectiveAddress + X_Register;

    uint8_t value = memoryModule.readMemory(newEffectiveAddress);

    return {newEffectiveAddress, oldEffectiveAddress, value};
}
CPU::AddressValue CPU::getAbsoluteYAddress() {
    PC++;
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(PC++);
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(PC++);
    uint16_t oldEffectiveAddress = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;
    uint16_t newEffectiveAddress = oldEffectiveAddress + Y_Register;

    uint8_t value = memoryModule.readMemory(newEffectiveAddress);

    return {newEffectiveAddress, oldEffectiveAddress, value};
}
uint8_t CPU::getIndirectAddress() {

}
CPU::AddressValue CPU::getIndirectXAddress() {
    PC++;
    uint8_t zeroPageAddress = memoryModule.readMemory(PC++);
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(zeroPageAddress + X_Register); // Low byte of effective address
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(zeroPageAddress + X_Register + 1); // High byte of effective address
    uint16_t effectiveAddress = (effectiveAddressHighByte << 8) | effectiveAddressLowByte; // Combine to form the effective address

    uint8_t value = memoryModule.readMemory(effectiveAddress);

    return {effectiveAddress, 0, value};
}
CPU::AddressValue CPU::getIndirectYAddress() {
    PC++;
    uint8_t zeroPageAddress = memoryModule.readMemory(PC++);
    uint8_t effectiveAddressLowByte = memoryModule.readMemory(zeroPageAddress);
    uint8_t effectiveAddressHighByte = memoryModule.readMemory(zeroPageAddress + 1);
    uint16_t oldEffectiveAddress = (effectiveAddressHighByte << 8) | effectiveAddressLowByte;
    uint16_t newEffectiveAddress = oldEffectiveAddress + Y_Register;

    uint8_t value = memoryModule.readMemory(newEffectiveAddress);

    return {newEffectiveAddress, oldEffectiveAddress, value};
}

// Lookup Table --------------------------------------------------------------------------------------------------------

void CPU::initializeLookupTable() {
    // Initialize all to a default instruction handler
    lookupTable.fill(&CPU::unimplementedInstruction);

    lookupTable[0x00] = &CPU::brk;
    lookupTable[0x01] = &CPU::oraIndirectX;
    lookupTable[0x05] = &CPU::oraZpg;
    lookupTable[0x06] = &CPU::aslZpg;
    lookupTable[0x08] = &CPU::php;
    lookupTable[0x09] = &CPU::oraImmediate;
    lookupTable[0x0A] = &CPU::aslA;
    lookupTable[0x0D] = &CPU::oraAbsolute;
    lookupTable[0x0E] = &CPU::aslAbsolute;

    lookupTable[0x10] = &CPU::bpl;
    lookupTable[0x11] = &CPU::oraIndirectY;
    lookupTable[0x15] = &CPU::oraZpgX;
    lookupTable[0x16] = &CPU::aslZpgX;
    lookupTable[0x18] = &CPU::clc;
    lookupTable[0x19] = &CPU::oraAbsoluteY;
    lookupTable[0x1D] = &CPU::oraAbsoluteX;
    lookupTable[0x1E] = &CPU::aslAbsoluteX;

    lookupTable[0x20] = &CPU::jsr;
    lookupTable[0x21] = &CPU::andIndirectX;
    lookupTable[0x24] = &CPU::bitZpg;
    lookupTable[0x25] = &CPU::andZpg;
    lookupTable[0x26] = &CPU::rolZpg;
    lookupTable[0x28] = &CPU::plp;
    lookupTable[0x29] = &CPU::andImmediate;
    lookupTable[0x2A] = &CPU::rolA;
    lookupTable[0x2C] = &CPU::bitAbsolute;
    lookupTable[0x2D] = &CPU::andAbsolute;
    lookupTable[0x2E] = &CPU::rolAbsolute;

    lookupTable[0x30] = &CPU::bmi;
    lookupTable[0x31] = &CPU::andIndirectY;
    lookupTable[0x35] = &CPU::andZpgX;
    lookupTable[0x36] = &CPU::rolZpgX;
    lookupTable[0x38] = &CPU::sec;
    lookupTable[0x39] = &CPU::andAbsoluteY;
    lookupTable[0x3D] = &CPU::andAbsoluteX;
    lookupTable[0x3E] = &CPU::rolAbsoluteX;

    lookupTable[0x40] = &CPU::rti;
    lookupTable[0x41] = &CPU::eorIndirectX;
    lookupTable[0x45] = &CPU::eorZpg;
    lookupTable[0x46] = &CPU::lsrZpg;
    lookupTable[0x48] = &CPU::pha;
    lookupTable[0x49] = &CPU::eorImmediate;
    lookupTable[0x4A] = &CPU::lsrA;
    lookupTable[0x4C] = &CPU::jmpAbsolute;
    lookupTable[0x4D] = &CPU::eorAbsolute;
    lookupTable[0x4E] = &CPU::lsrAbsolute;

    lookupTable[0x50] = &CPU::bvc;
    lookupTable[0x51] = &CPU::eorIndirectY;
    lookupTable[0x55] = &CPU::eorZpgX;
    lookupTable[0x56] = &CPU::lsrZpgX;
    lookupTable[0x58] = &CPU::cli;
    lookupTable[0x59] = &CPU::eorAbsoluteY;
    lookupTable[0x5D] = &CPU::eorAbsoluteX;
    lookupTable[0x5E] = &CPU::lsrAbsoluteX;

    lookupTable[0x60] = &CPU::rts;
    lookupTable[0x61] = &CPU::adcIndirectX;
    lookupTable[0x65] = &CPU::adcZpg;
    lookupTable[0x66] = &CPU::rorZpg;
    lookupTable[0x68] = &CPU::pla;
    lookupTable[0x69] = &CPU::adcImmediate;
    lookupTable[0x6A] = &CPU::rorA;
    lookupTable[0x6C] = &CPU::jmpIndirect;
    lookupTable[0x6D] = &CPU::adcAbsolute;
    lookupTable[0x6E] = &CPU::rorAbsolute;

    lookupTable[0x70] = &CPU::bvs;
    lookupTable[0x71] = &CPU::adcIndirectY;
    lookupTable[0x75] = &CPU::adcZpgX;
    lookupTable[0x76] = &CPU::rorZpgX;
    lookupTable[0x78] = &CPU::sei;
    lookupTable[0x79] = &CPU::adcAbsoluteY;
    lookupTable[0x7D] = &CPU::adcAbsoluteX;
    lookupTable[0x7E] = &CPU::rorAbsoluteX;

    lookupTable[0x81] = &CPU::staIndirectX;
    lookupTable[0x84] = &CPU::styZpg;
    lookupTable[0x85] = &CPU::staZpg;
    lookupTable[0x86] = &CPU::stxZpg;
    lookupTable[0x88] = &CPU::dey;
    lookupTable[0x8A] = &CPU::txa;
    lookupTable[0x8C] = &CPU::styAbsolute;
    lookupTable[0x8D] = &CPU::staAbsolute;
    lookupTable[0x8E] = &CPU::stxAbsolute;

    lookupTable[0x90] = &CPU::bcc;
    lookupTable[0x91] = &CPU::staIndirectY;
    lookupTable[0x94] = &CPU::styZpgX;
    lookupTable[0x95] = &CPU::staZpgX;
    lookupTable[0x96] = &CPU::stxZpgY;
    lookupTable[0x98] = &CPU::tya;
    lookupTable[0x99] = &CPU::staAbsoluteY;
    lookupTable[0x9A] = &CPU::txs;
    lookupTable[0x9D] = &CPU::staAbsoluteX;

    lookupTable[0xA0] = &CPU::ldyImmediate;
    lookupTable[0xA1] = &CPU::ldaIndirectX;
    lookupTable[0xA2] = &CPU::ldxImmediate;
    lookupTable[0xA4] = &CPU::ldyZpg;
    lookupTable[0xA5] = &CPU::ldaZpg;
    lookupTable[0xA6] = &CPU::ldxZpg;
    lookupTable[0xA8] = &CPU::tay;
    lookupTable[0xA9] = &CPU::ldaImmediate;
    lookupTable[0xAA] = &CPU::tax;
    lookupTable[0xAC] = &CPU::ldyAbsolute;
    lookupTable[0xAD] = &CPU::ldaAbsolute;
    lookupTable[0xAE] = &CPU::ldxAbsolute;

    lookupTable[0xB0] = &CPU::bcs;
    lookupTable[0xB1] = &CPU::ldaIndirectY;
    lookupTable[0xB4] = &CPU::ldyZpgX;
    lookupTable[0xB5] = &CPU::ldaZpgX;
    lookupTable[0xB6] = &CPU::ldxZpgY;
    lookupTable[0xB8] = &CPU::clv;
    lookupTable[0xB9] = &CPU::ldaAbsoluteY;
    lookupTable[0xBA] = &CPU::tsx;
    lookupTable[0xBC] = &CPU::ldyAbsoluteX;
    lookupTable[0xBD] = &CPU::ldaAbsoluteX;
    lookupTable[0xBE] = &CPU::ldxAbsoluteY;

    lookupTable[0xC0] = &CPU::cpyImmediate;
    lookupTable[0xC1] = &CPU::cmpIndirectX;
    lookupTable[0xC4] = &CPU::cpyZpg;
    lookupTable[0xC5] = &CPU::cmpZpg;
    lookupTable[0xC6] = &CPU::decZpg;
    lookupTable[0xC8] = &CPU::iny;
    lookupTable[0xC9] = &CPU::cmpImmediate;
    lookupTable[0xCA] = &CPU::dex;
    lookupTable[0xCC] = &CPU::cpyAbsolute;
    lookupTable[0xCD] = &CPU::cmpAbsolute;
    lookupTable[0xCE] = &CPU::decAbsolute;

    lookupTable[0xD0] = &CPU::bne;
    lookupTable[0xD1] = &CPU::cmpIndirectY;
    lookupTable[0xD5] = &CPU::cmpZpgX;
    lookupTable[0xD6] = &CPU::decZpgX;
    lookupTable[0xD8] = &CPU::cld;
    lookupTable[0xD9] = &CPU::cmpAbsoluteY;
    lookupTable[0xDD] = &CPU::cmpAbsoluteX;
    lookupTable[0xDE] = &CPU::decAbsoluteX;

    lookupTable[0xE0] = &CPU::cpxImmediate;
    lookupTable[0xE1] = &CPU::sbcIndirectX;
    lookupTable[0xE4] = &CPU::cpxZpg;
    lookupTable[0xE5] = &CPU::sbcZpg;
    lookupTable[0xE6] = &CPU::incZpg;
    lookupTable[0xE8] = &CPU::inx;
    lookupTable[0xE9] = &CPU::sbcImmediate;
    lookupTable[0xEA] = &CPU::nop;
    lookupTable[0xEC] = &CPU::cpxAbsolute;
    lookupTable[0xED] = &CPU::sbcAbsolute;
    lookupTable[0xEE] = &CPU::incAbsolute;

    lookupTable[0xF0] = &CPU::beq;
    lookupTable[0xF1] = &CPU::sbcIndirectY;
    lookupTable[0xF5] = &CPU::sbcZpgX;
    lookupTable[0xF6] = &CPU::incZpgX;
    lookupTable[0xF8] = &CPU::sed;
    lookupTable[0xF9] = &CPU::sbcAbsoluteY;
    lookupTable[0xFD] = &CPU::sbcAbsoluteX;
    lookupTable[0xFE] = &CPU::incAbsoluteX;
}

void CPU::executeInstruction(uint8_t opcode) {
    auto instruction = lookupTable[opcode];
    (this->*instruction)();
}

void CPU::reset() {
    uint16_t resetVector = (memoryModule.readMemory(0xFFFD) << 8) | memoryModule.readMemory(0xFFFC);
    PC = resetVector;
    Accumulator = 0;
    X_Register = 0;
    Y_Register = 0;
    SP = 0xFF;
    SR = INTERRUPT_FLAG;
}

// Utility Functions ---------------------------------------------------------------------------------------------------
void CPU::setFlag(uint8_t flag, bool value) {
    if (value) {
        SR |= flag;     // Set the flag
    } else {
        SR &= ~flag;    // Clear the flag
    }
}
void CPU::updateNegativeFlag(uint8_t value) {
    // Update NEGATIVE_FLAG
    if (value & 0x80) { // Check if the 7th bit is set
        setFlag(NEGATIVE_FLAG, true); // Set NEGATIVE_FLAG
    } else {
        setFlag(NEGATIVE_FLAG, false); // Clear NEGATIVE_FLAG
    }
}
void CPU::updateOverflowFlag(uint8_t value) {
    // Update OVERFLOW_FLAG
    if (value & 0x40) {
        setFlag(OVERFLOW_FLAG, true);   // Set OVERFLOW_FLAG
    } else {
        setFlag(OVERFLOW_FLAG, false);  // Clear OVERFLOW_FLAG
    }
}
void CPU::updateZeroFlag(uint8_t value) {
    // Update ZERO_FLAG
    if (value == 0x00) {
        setFlag(ZERO_FLAG, true); // Set ZERO_FLAG
    } else {
        setFlag(ZERO_FLAG, false); // Clear ZERO_FLAG
    }
}
void CPU::updateCarryFlag(uint8_t value) {
    // Update CARRY_FLAG
    if (value & 0x80) {
        setFlag(CARRY_FLAG, true); // Set CARRY_FLAG
    } else {
        setFlag(CARRY_FLAG, false); // Clear CARRY_FLAG
    }
}

bool CPU::checkPageCrossing(const uint16_t oldAddress, const uint16_t newAddress) {
    // Check if the high byte of the old and new addresses are different
    return (oldAddress & 0xFF00) != (newAddress & 0xFF00);
}