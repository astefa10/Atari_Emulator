#include "memory.h"

Memory::Memory() {
    // RAM
    std::fill(std::begin(memory) + 0x0080, std::begin(memory) + 0x0100, 0x00);

    //TODO
    // TIA 0x00 - 0x7F
    // RIOT 0x280 - 0x2FF
    // Program 0x1000 - 0x1FFF
};

void Memory::pushStack(uint8_t &stackPointer, uint8_t value) {
    if (stackPointer > 0x00) {
        memory[0x0100 + stackPointer] = value;
        stackPointer--;
    } else {
        std::cerr << "Stack underflow" << std::endl;
    }
}

uint8_t Memory::popStack(uint8_t &stackPointer) const {
    if (stackPointer < 0xFF) {
        stackPointer++;
        return memory[0x0100 + stackPointer];
    } else {
        std::cerr << "Stack overflow" << std::endl;
    }
}

uint8_t Memory::readMemory(uint16_t address) const {
    return memory[address];
}

void Memory::writeMemory(uint16_t address, uint8_t value) {
    memory[address] = value;
}