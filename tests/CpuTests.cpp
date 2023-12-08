#include <gtest/gtest.h>
#include "cpu.h"
#include "memory.h"

class CpuTest : public ::testing::Test {
protected:
    CPU cpu;

    void SetUp() override {
        cpu.reset();
    }
};