#ifndef MEMORY_H
#define MEMORY_H

#include <cstdint>
#include <array>
#include <iostream>

class Memory {
public:
    Memory();
    void pushStack(uint8_t& stackPointer, uint8_t value);
    uint8_t popStack(uint8_t& stackPointer) const;
    uint8_t readMemory(uint16_t address) const;
    void writeMemory(uint16_t address, uint8_t value);

private:
    std::array<uint8_t, 8192> memory;

};

#endif //MEMORY_H
