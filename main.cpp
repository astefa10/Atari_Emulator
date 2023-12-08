#include "cpu.h"
#include <iostream>

int main() {
    CPU cpu;
    //cpu.executeInstruction(0x00);
    //cpu.executeInstruction(0x01);
    //cpu.executeInstruction(0x05);
    cpu.executeInstruction(0x06);
    std::cout << "test" << std::endl;
}