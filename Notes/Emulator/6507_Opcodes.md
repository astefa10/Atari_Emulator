#CPU

| Hex  | Mnemonic | Description                       | Bytes | Cycles|
|------|----------|-----------------------------------|----|---|
| 0x00 | BRK impl     | Force Interrupt (Implied) | 1 | 7|
| 0x01 | ORA X, ind      | OR with Accumulator (X, Indirect) |2 | 6|
| <span class='illegal-opcode'>0x02</span> | JAM      | Jams the CPU                      |
| <span class='illegal-opcode'>0x03</span> | SLO X, ind | Store ASL + ORA (Indirect, X) |
| <span class='illegal-opcode'>0x04</span> | NOP zpg | No Operation (Zero Page) |
| 0x05 | ORA zpg     | OR with Accumulator (Zero Page)   |2 | 3|
| 0x06 | ASL zpg      | Arithmetic Shift Left (Zero Page) |2 | 5|
| <span class='illegal-opcode'>0x07</span> | SLO zpg | Store ASL + ORA (Zero Page) |
| 0x08 | PHP impl     | Push Processor Status             |1|3|
| 0x09 | ORA #     | OR with Accumulator (Immediate)   |2 | 2|
| 0x0A | ASL A     | Arithmetic Shift Left (Accumulator) |1|2|
| <span class='illegal-opcode'>0x0B</span> | ANC # | AND + Set C (Immediate)|
| <span class='illegal-opcode'>0x0C</span> | NOP abs | No Operation (Absolute) |
| 0x0D | ORA abs    | OR with Accumulator (Absolute)    |3 | 4|
| 0x0E | ASL abs     | Arithmetic Shift Left (Absolute)  |3|6|
| <span class='illegal-opcode'>0x0F</span> | SLO abs  | Store ALS + ORA (Absolute) |
| --- | ---| --- |
| 0x10 | BPL rel     | Branch if Plus (Relative)|2|2**|
| 0x11 | ORA ind, Y     | Or with Accumulator (Indirect, Y) |2|5*|
| <span class='illegal-opcode'>0x12</span> | JAM | Jams the CPU |
| <span class='illegal-opcode'>0x13</span> | SLO ind, Y | Store ASL + ORA (Indirect, Y) |
| <span class='illegal-opcode'>0x14</span> | NOP zpg, X | No Operation (Zero Page, X) |
| 0x15 | ORA zpg, X     | OR with Accumulator (Zero Page, X)   |2 | 4|
| 0x16 | ASL zpg, X    | Arithmetic Shift Left (Zero Page, X) |2|6|
| <span class='illegal-opcode'>0x17</span> | SLO zpg, X | Store ASL + ORA (Zero Page, X) |
| 0x18 | CLC impl     | Clear Carry (Implied) |1|2|
| 0x19 | ORA abs, Y     | OR with Accumulator (Absolute, Y) |3|4*|
| <span class='illegal-opcode'>0x1A</span> | NOP impl | No Operation (Implied) |
| <span class='illegal-opcode'>0x1B</span> | SLO abs, Y | Store ASL + ORA (Absolute, Y) |
| <span class='illegal-opcode'>0x1C</span> | NOP abs, X | No Operation (Absolute, Y) |
| 0x1D | ORA abs, X     | OR with Accumulator (Absolute, X)|3|4*|
| 0x1E | ASL abs, X     | Arithmetic Shift Left (Absolute, X) |3|7|
| <span class='illegal-opcode'>0x1F</span> | SLO abs, X | Store ASL + ORA (Absolute, X) |
| --- | --- | --- |
| 0x20 | JSR abs    | Jump to Subroutine (Absolute)|3|6|
| 0x21 | AND X, ind     | AND with Accumulator (X, Indirect) |2|6|
| <span class='illegal-opcode'>0x22</span> | JAM | Jams the CPU||
| <span class='illegal-opcode'>0x23</span> | RLA X, ind | ROL + AND (X, Indirect) | 
| 0x24 | BIT zpg | Bit Test (Zero Page) |2|3|
| 0x25 | AND zpg | AND with Accumulator (Zero Page) | 2|3|
| 0x26 | ROL zpg | Rotate Left (Zero Page) |2|5|
| <span class='illegal-opcode'>0x27</span> | RLA zpg | ROL + AND (Zero Page) |
| 0x28 | PLP impl | Pull Processor Status From Stack (Implied) |1|4|
| 0x29 | AND # | AND with Accumulator (Immediate) |2|2|
| 0x2A | ROL A | Rotate Left (Accumulator) |1|2|
| <span class='illegal-opcode'>0x2B</span> | ANC # | AND + Set C (Immediate) |
| 0x2C | BIT abs | Bit Test (Absolute) |3|4
| 0x2D | AND abs | AND with Accumulator (Absolute) |3|4|
| 0x2E | ROL abs | Rotate Left (Absolute) |3|6|
| <span class='illegal-opcode'>0x2F</span> | RLA abs | ROL + AND (Absolute) |
| ...  | ...      | ...                               |
| 0x30 | BMI rel     | Branch if Minus (Relative)     |2|2**|
| 0x31 | AND ind, Y | AND with Accumulator (Indirect, Y) |2|5*|
| <span class='illegal-opcode'>0x32</span> | JAM | Jams the CPU |
| <span class='illegal-opcode'>0x33</span> | RLA ind, Y | ROL + AND (Indirect, Y) |
| <span class='illegal-opcode'>0x34</span> | NOP zpg, X | No Operation (Zero Page, X) |
| 0x35 | AND zpg, X | AND with Accumulator (Zero Page, X) |2|4|
| 0x36 | ROL zpg, X | Rotate Left (Zero Page, X) |2|6|
| <span class='illegal-opcode'>0x37</span> | RLA zpg, X | ROL + AND (Zero Page, X) |
| 0x38 | SEC impl | Set Carry (Implied) |1|2|
| 0x39 | AND abs, Y | AND with Accumulator (Absolute, Y) |3|4*|
| <span class='illegal-opcode'>0x3A</span> | NOP impl | No Operation (Implied) |
| <span class='illegal-opcode'>0x3B</span> | RLA abs, Y | ROL + AND (Absolute, Y) |
| <span class='illegal-opcode'>0x3C</span> | NOP abs, X | No Operation (Absolute, X) |
| 0x3D | AND abs, X | AND with Accumulator (Absolute, X) |3|4*|
| 0x3E | ROL abs, X | Rotate Left (Absolute, X) |3|7|
| <span class='illegal-opcode'>0x3F</span> | RLA abs, X | ROL + AND (Absolute, X) | 
| ---  | ---      | ---                               |
| 0x40 | RTI impl     | Return from Interrupt (Implied) |1|6|
| 0x41 | EOR X, ind | XOR with Accumulator (X, Indirect) | 2|6|
| <span class='illegal-opcode'>0x42</span> | JAM | Jams the CPU|
| <span class='illegal-opcode'>0x43</span> | SRE X, ind | Store LSR + EOR (X, Indirect) |
| <span class='illegal-opcode'>0x44</span> | NOP zpg | No Operatino (Zero Page) |
| 0x45 | EOR zpg | XOR with Accumulator (Zero Page) |2|3|
| 0x46 | LSR zpg | Logical Shift Right (Zero Page) |2|5|
| <span class='illegal-opcode'>0x47</span> | SRE zpg | Store LSR + EOR (Zero Page) |
| 0x48 | PHA impl | Push Accumlator onto Stack (Implied) |1|3|
| 0x49 | EOR # | XOR with Accumulator (Immediate) |2|2|
| 0x4A | LSR A | Logical Shift Right (Accumulator) |1|2|
| <span class='illegal-opcode'>0x4B</span> | ALR # | AND + LSR (Immediate) |
| 0x4C | JMP abs | Jump (Absolute) |3|3|
| 0x4D | EOR abs | XOR with Accumulator (Absolute) |3|4|
| 0x4E | LSR abs | Logical Shift Right (Absolute) |3|6|
| <span class='illegal-opcode'>0x4F</span> | SRE abs | Store LSR + EOR (Absolute) |
| --- | --- | --- |
| 0x50 | BVC rel | Branch on Overflow Clear (Relative) | 2|2**|
| 0x51 | EOR ind, Y | XOR with Accumulator (Indirect, Y) |2|5*|
|<span class='illegal-opcode'>0x52</span> | JAM | Jams the CPU | 
|<span class='illegal-opcode'>0x53</span> | SRE ind, Y | Store LSR + EOR (Indirect, Y)|
|<span class='illegal-opcode'>0x54</span> | NOP zpg, X | No Operation (Zero Page, X) | 
|0x55 | EOR zpg, X | XOR with Accumulator (Zero Page, X) |2|4|
|0x56 | LSR zpg, X | Logical Shift Right (Zero Page, X) |2|6| 
|<span class='illegal-opcode'>0x57</span> | SRE zpg, X | Store LSR + EOR (Zero Page, X) | 
|0x58 | CLI impl | Clear interrupt disable (Implied) |1|2|
|0x59 | EOR abs, Y | XOR with Accumulator (Absolute, Y)|3|4*|
|<span class='illegal-opcode'>0x5A</span> | NOP impl | No Operation (Implied)|
|<span class='illegal-opcode'>0x5B</span> | SRE abs, Y | Store LSR + EOR (Absolute, Y)|
|<span class='illegal-opcode'>0x5C</span> | NOP abs, X | No Operation (Absolute, X)|
|0x5D | EOR abs, X | XOR with Accumulator (Absolute, X)|3|4*|
|0x5E | LSR abs, X | Logical Shift Right (Absolute, X) |3|7|
|<span class='illegal-opcode'>0x5F</span> | SRE abs, X | Store LSR + EOR (Absolute, X) | 
| ---  | ---      | ---                               |
| 0x60 | RTS impl     | Return from Subroutine (Implied)|1|6|
| 0x61 | ADC X, ind | Add with Carry (X, Indirect) |2|6|
|<span class='illegal-opcode'>0x62</span> | JAM | Jams the CPU |
|<span class='illegal-opcode'>0x63</span> | RRA X, ind | ROR + ADC (X, Indirect) |
|<span class='illegal-opcode'>0x64</span> | NOP zpg | No Operation (Zero Page) ||
|0x65 | ADC zpg | Add with Carry (Zero Page) | 2|3|
|0x66 | ROR zpg | Rotate Right (Zero Page) | 2|5|
|<span class='illegal-opcode'>0x67</span> | RRA zpg | ROR + ADC (Zero Page) |
|0x68 | PLA impl | Pull Accumulator from Stack (Implied) | 1|4|
|0x69 | ADC # | Add with Carry (Immediate) |2|2|
|0x6A | ROR A | Rotate Right (Accumulator) |1|2|
|<span class='illegal-opcode'>0x6B</span> | ARR # | AND + ROR (Immediate) |
|0x6C | JMP ind | Jump (Indirect)|3|5|
| 0x6D | ADC abs | Add with Carry (Absolute) |3|4|
|0x6E | ROR abs | Rotate Right (Absolute) | 3|6|
|<span class='illegal-opcode'>0x6F</span> | RRA abs | ROR + ADC (Absolute) |
| ---  | ---      | ---                               |
|0x70 | BVS rel | Branch on Overflow set (Relative) | 2|2**|
|0x71 | ADC ind, Y | Add with Carry (Indirect, Y)  |2|5*|
|<span class='illegal-opcode'>0x72</span> | JAM | Jams the CPU | 
| <span class='illegal-opcode'>0x73</span> | RRA ind, Y | ROR + ADC (Indirect, Y)|
|<span class='illegal-opcode'>0x74</span> | NOP zpg, X | No Operation (Zero Page, X)|
|0x75 | ADC zpg, X  | Add with Carry (Zero Page, X)|2|4|
|0x76 | ROR zpg, X | Rotate Right (Zero Page, X) |2|6|
|<span class='illegal-opcode'>0x77</span> | RRA zpg, X | ROR + ADC (Zero Page, X)|
| 0x78 | SEI impl     | Set Interrupt Disable (Implied)|1|2|
|0x79 | ADC abs, Y | Add with Carry (Absolute, Y)|3|4*|
|0x7D | ADC abs, X | Add with Carry (Absolute, X) |3|4*|
|0x7E | ROR abs, X | Rotate Right (Absolute, X)|3|7|
| ---  | ---      | ---                               |
|0x81 | STA X, ind | Store Accumulator (X, Indirect)|2|6|
| 0x84 | STY zpg     | Store Y Register (Zero Page)|2|3|
|0x85 | STA zpg | Store Accumulator (Zero Page) | 2|3|
|0x86 | STX zpg | Store X (Zero Page) | 2|3|
|0x88 | DEY impl | Decrement Y (Implied) | 1|2|
|0x8A | TXA impl | Transfer X to Accumulator (Implied) | 1|2|
|0x8C | STY abs | Store Y (Absolute) |3|4|
|0x8D | STA abs | Store Accumulator (Absolute) |3|4|
|0x8E | STX abs | Store X (Absolute) |3|4|
| ---  | ---      | ---                               |
| 0x90 | BCC rel     | Branch on Carry Clear|2|2**|
|0x91 | STA ind, Y | Store Accumulator (Indirect, Y) | 2|6|
|0x94 | STY zpg, X | Store Y (Zero Page, X)|2|4|
|0x95 | STA zpg, X | Store Accumulator (Zero Page, X)|2|4|
|0x96 | STX zpg, Y | Store X (Zero Page, Y)|2|4|
|0x98 | TYA impl | Transfer Y to Accumulator (Implied) |1|2|
|0x99 | STA abs, Y | Store Accumulator (Absolute, Y) |3|5|
|0x9A | TXS impl | Transfer X to Stack Pointer (Implied) |1|2|
|0x9D | STA abs, X | Store Accumulator (Absolute, X)|3|5|
| ---  | ---      | ---                               |
| 0xA0 | LDY #     | Load Y Register (Immediate)|2|2|
|0xA1 | LDA X, ind | Load Accumulator (X, Indirect) |2|6|
|0xA2 | LDX # | Load X (Immediate)|2|2|
|0xA4 | LDY zpg | Load Y (Zero Page) |2|3|
|0xA5 | LDA zpg | Load Accumulator (Zero Page) |2|3|
|0xA6 | LDX zpg | Load X (Zero Page)|2|3|
|0xA8 | TAY impl | Transfer Accumulator to Y (Implied)|1|2|
|0xA9 | LDA # | Load Accumulator (Immediate)|2|2|
|0xAA | TAX impl | Transfer Accumulator to X (Immediate) |1|2|
|0xAC | LDY abs | Load Y (Absolute)|3|4|
|0xAD | LDA abs | Load Accumulator (Absolute)|3|4|
|0xAE | LDX abs | Load X (Absolute)|3|4|
| ---  | ---      | ---                               |
| 0xB0 | BCS rel     | Branch if Carry Set (Relative)|2|2**|
|0xB1 | LDA ind, Y | Load Accumulator (Indirect, Y) | 2|5*|
|0xB4 | LDY zpg, X | Load Y (Zero Page, X) |2|4|
|0xB5 | LDA zpg, X | Load Accumulator (Zero Page, X)|2|4|
|0xB6 | LDX zpg, Y | Load X (Zero Page, Y)|2|4|
|0xB8 | CLV impl | Clear Overflow (Implied) |1|2|
|0xB9 | LDA abs, Y | Load Accumulator (Absolute, Y)|3|4*|
|0xBA | TSX impl | Transfer Stack Pointer to X (Implied) |1|2|
|0xBC | LDY abs, X | Load Y (Absolute, X)|3|4*|
|0xBD | LDA abs, X | Load Accumulator (Absolute, X)|3|4*|
|0xBE | LDX abs, X | Load X (Absolute, X)|3|4*|
| ---  | ---      | ---                               |
| 0xC0 | CPY #     | Compare with Y (Immediate)|2|2|
|0xC1 | CMP X, ind | Compare with Accumulator (X, Indirect) |2|6|
|0xC4 | CPY zpg | Compare with Y (Zero Page) |2|3|
|0xC5 | CMP zpg | Compare with Accumulator (Zero Page) |2|3|
|0xC6 | DEC zpg | Decrement (Zero Page)|2|5|
|0xC8 | INY impl | Increment Y (Implied) |1|2|
|0xC9 | CMP # | Compare with Accumulator (Immediate) |2|2|
|0xCA | DEX impl | Decrement X (Implied)|1|2|
|0xCC | CPY abs | Compare with Y (Absolute)|3|4|
|0xCD | CMP abs | Compare with Accumulator (Absolute)|3|4*|
|0xCE | DEC abs | Decrement (Absolute)|3|6|
| ---  | ---      | ---                               |
| 0xD0 | BNE rel     | Branch if Not Equal (Relative)|2|2**|
|0xD1 | CMP ind, Y | Compare with Accumulator (Indirect, Y)|2|5*|
|0xD5 | CMP zpg, X | Compare with Accumulator (Zero Page, X) |2|4|
|0xD6 | DEC zpg, X | Decrement (Zero Page, X) |2|6|
|0xD8 | CLD impl | Clear Decimal (Implied) |1|2|
|0xD9 | CMP abs, Y | Compare with Accumulator (Absolute, Y) |3|4*|
|0xDD | CMP abs, X | Compare with Accumulator (Absolute, X) |3|4*|
|0xDE | DEC abs, X | Decrement (Absolute, X)|3|7|
| ---  | ---      | ---                               |
| 0xE0 | CPX #     | Compare X Register (Immediate)    |2|2|
|0xE1 | SBC X, ind | Subtract with Carry (X, Indirect) |2|6|
|0xE4 | CPX zpg | Compare with X (Zero Page) |2|3|
|0xE5 | SBC zpg | Subtract with Carry (Zero Page) |2|3|
|0xE6 | INC zpg | Increment (Zero Page)|2|5|
|0xE8 | INX impl | Increment X (Implied) |1|2|
|0xE9 | SBC # | Subtract with Carry (Immediate)|2|2|
|0xEA | NOP impl | No Operation (Implied) |1|2|
|0xEC | CPX abs | Compare with X (Absolute) |3|4|
|0xED | SBC abs | Subtract with Carry (Absolute) |3|4|
|0xEE | INC abs | Increment (Absolute) |3|6|
| ---  | ---      | ---                               |
| 0xF0 | BEQ rel     | Branch if Equal (Relative) |2|2**|
|0xF1 | SBC ind, Y | Subtract with Carry (Indirect, Y) |2|5*|
|0xF5 | SBC zpg, X | Subtract with Carry (Zero Page, X) |2|4|
|0xF6 | INC zpg, X | Increment (Zero Page, X) |2|6|
|0xF8 | SED impl | Set Decimal (Implied) |1|2|
|0xF9 | SBC abs, Y | Subtract with Carry (Absolute, Y)|3|4*|
|0xFD | SBC abs, X | Subtract with Carry (Absolute, X)|3|4*|
|0xFE | INC abs, X | Increment (Absolute, X)|3|7|


*
    add 1 to cycles if page boundary is crossed
**
    add 1 to cycles if branch occurs on same page
    add 2 to cycles if branch occurs to different page

Legend to Flags:

+
    modified
-
    not modified
1
    set
0
    cleared
M6
    memory bit 6
M7
    memory bit 7

Note on assembler syntax:
Some assemblers employ "OPC *oper" or a ".b" extension
to the mneomonic for forced zeropage addressing.
### Load/Store Operations

- `LDA` - Load Accumulator
- `LDX` - Load X Register
- `LDY` - Load Y Register
- `STA` - Store Accumulator
- `STX` - Store X Register
- `STY` - Store Y Register

### Arithmetic

- `ADC` - Add with Carry
- `SBC` - Subtract with Carry

### Bitwise Operations

- `AND` - Logical AND
- `ORA` - Logical Inclusive OR
- `EOR` - Exclusive OR
- `ASL` - Arithmetic Shift Left
- `LSR` - Logical Shift Right
- `ROL` - Rotate Left
- `ROR` - Rotate Right

### Branch Instructions

- `BCC` - Branch if Carry Clear
- `BCS` - Branch if Carry Set
- `BEQ` - Branch if Equal
- `BMI` - Branch if Minus
- `BNE` - Branch if Not Equal
- `BPL` - Branch if Positive
- `BVC` - Branch if Overflow Clear
- `BVS` - Branch if Overflow Set

### Control Instructions

- `BRK` - Force Break
- `RTI` - Return from Interrupt
- `JSR` - Jump to Subroutine
- `RTS` - Return from Subroutine
- `JMP` - Jump

### Status Flag Changes

- `CLC` - Clear Carry Flag
- `SEC` - Set Carry Flag
- `CLI` - Clear Interrupt Disable
- `SEI` - Set Interrupt Disable
- `CLV` - Clear Overflow Flag
- `CLD` - Clear Decimal Mode
- `SED` - Set Decimal Mode

### Transfer Instructions

- `TAX` - Transfer Accumulator to X
- `TXA` - Transfer X to Accumulator
- `TAY` - Transfer Accumulator to Y
- `TYA` - Transfer Y to Accumulator
- `TSX` - Transfer Stack Pointer to X
- `TXS` - Transfer X to Stack Pointer

### Stack Operations

- `PHA` - Push Accumulator
- `PLA` - Pull Accumulator
- `PHP` - Push Processor Status
- `PLP` - Pull Processor Status




