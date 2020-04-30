# :warning: Section 3: Variables, Literals, Figurative Constants, Datatypes, Levels, & More
## 11. Variables, Literals, Figurative Constants
**variable**
- max length 30 characters
- can contain letters, numbers, and hyphens
**literals**
- constants
- directly hard-coded in the program
- 2 types:
	- numeric literal - max 18 characters
	- alphanumeric (non-numeric) literal - max 160 characters and must start and end in quotes
**figurative constants** - built into COBOL
ex.
```
ZERO / ZEROES / ZEROS
SPACE / SPACES
HIGH-VALUE / HIGH-VALUES
LOW-VALUE / LOW-VALUES
QUOTE / QUOTES
ALL
NULL'
```
- don't use HIGH-VALUE or LOW-VALUE with numeric fields!
```
WORKING-STORAGE SECTION.
  01 WS-VARIABLE   PIC X(30).
  01 WS-LITERAL-1  PIC 9(03) VALUE 123.
  01 WS-LITERAL-2  PIC A(15) VALUE "I AM A LITERAL".
  01 WS-LITERAL-3  PIC X(03) VALUE 'X23'.
PROCEDURE DIVISION.
MAIN-PARA.
  MOVE SPACES TO WS-VARIABLE.
  MOVE "VARIABLE, LITERALS AND MORE" TO WS-VARIABLE
  DISPLAY WS-VARIABLE.
  DISPLAY WS-LITERAL-1.
  DISPLAY WS-LITERAL-2.
  DISPLAY WS-LITERAL-3.
STOP RUN.
```
## 12. Data Types in COBOL
**data types**
- denoted by picture `PIC` clause
- `PIC` clause provides data type and length
- **numeric** - denoted by `9`, 0-9, max length of 18
- **alphabetic** - denoted by `A`, A-Z plus space, max length of 255
- **alphanumeric** - combination of numeric and alphabetic, denoted by `X`

numeric data type of length two ex. `PIC 9(2)`

`S` - **signed data type** - links a sign to a number, if present the number is a signed number, if not, it's unsigned

`+` - used to print plus sign
`-` - used to print minus sign

`V` - implied decimal, doesn't hold memory space and not used for computations

`PIC 9(4)V99`

given the value of 123456, it will display as 123456 but compute as 1234.56

`.` - decimal point, only used for display and not in calculations

`PIC 9(4).99`

given the value 123456 will display as 1234.56 but compute as 123456

`Z` - used to suppress only leading zeros with blanks, doesn't affect non-zeros

`PIC ZZ99.99`

given the value 0012.34 will become bb12.34

`,` - used to insert a comma into the data item at a particular position

`$` - used to insert a dollar symbol at the first position

- `PIC 999` is the same as `PIC 9(3)`

## 13. General Level Number in COBOL
- data items are defined by levels
- 01 is the topmost level
- ex. `01 EMP-ID PIC 9(5)`

 group data item ex.
 ```
01 EMP-RECORD.
  05 EMP-ID PIC 9(5).
  05 EMP-NAME.
    10 FIRST-NAME PIC A(15).
    10 LAST-NAME PIC A(15).
```

- as you move down, level number must increase, generally going 1, 5, 10, 15, etc.
- generally only use levels 1-49

66, 77, and 88 are special purpose levels

**66** is used for RENAMES clause (which doesn't include a pictures clause)
ex. 
```
01 EMP-REC.
  05 EMP-ID PIC 9(5).
  05 EMP-NAME PIC A(10).
  05 EMP-BIRTH-DATE PIC X(10).
66 EMP-DETAIL RENAMES EMP-ID THRU EMP-NAME.
```

**77** avoid using, it's only for individual data items or elementary data items, and cannot be further subdivided

**88** used for conditional processing, must be coded under a group item, uses boolean elif logic

ex.
```
01 CHECK-DAY.
  05 DAY PIC X(3).
    88 MONDAY    VALUE 'MON'.
    88 TUESDAY   VALUE 'TUE'.
    88 WEDNESDAY VALUE 'WED'.
    88 THURSDAY  VALUE 'THU'.
    88 FRIDAY    VALUE 'FRI'.
    88 SATURDAY  VALUE 'SAT'.
    88 SUNDAY    VALUE 'SUN'.
```

## 15. Exploring a Program with Various Levels
```
DATA DIVISION.
  WORKING -STORAGE SECTION.
    01 WS-DISPLAY PIC X(25).
    01 STUDENTS. <group variable>
      05 STUDENT-ID PIC 9(5). <elementary variable>
      05 FILLER PIC X(01). <elementary variable>
      05 STUDENT-NAME. <group variable>
        10 FNAME PIC A(08). <elementary variable>
        10 LNAME PIC A(06).
      STUDENT-DEPT PIC X(3).
        88 COMPUTER    VALUE 'CMP'.
        88 ELECTRONICS VALUE 'EEE'.
        88 MECHANICAL  VALUE 'MEC'.
        88 CIVIL       VALUE 'CIV'.
    66 STUDENT-DETAILS RENAMES STUDENT-ID THRU STUDENT-NAME.
```

a picture clause makes something an elementary variable

## 16. Display and more in COBOL
- put `hi cobol` in command line to display colors on screen

```
PROCEDURE DIVISION.
MAIN-PARA.
  MOVE "HELLO WORLD" TO WS-HELLO.
  DISPLAY WS-HELLOW.
  STOP RUN.
```
use `MOVE` to assign a value to a variable
use `DISPLAY` to display in the output

three types of `MOVE`:
1. simple move
2. substring move (reverencing modification
3. corresponding move

## 17. ACCEPT IN COBOL
- used to accept the value from the JCL or a system defined value

syntax: `ACCEPT IDENTIFIER FROM <NAME>`
ex. 
```
ACCEPT EMPLOYEE-DET.
ACCEPT CURRENT-DATE FROM DATE
ACCEPT CURRTIME FROM TIME
```

## 18. Relationship of Accept to SYSIN & SYSOUT Parameters
**sysin parameter**
 - optional
- way to pass input data to the program
- to accept data in the program need 'ACCEPT' statement

syntax:
```
// SYSIN DD*
<VALUES>
/ *
```
or
```
// SYSIN DD DATA
<VALUES>
//*
```

- another way of passing data to the program is called PARM
- if both SYSIN and PARM are in JCL, PARM overrides SYSIN

**SYSOUT parameter** 
- optional
- used to display the output int he SPOOL area or to an output class

syntax:
`// DDNAME DD SYSOUT=*`
OR
`// DDNAME DD SYSOUT=P`
- p denotes any valid class
 

        
