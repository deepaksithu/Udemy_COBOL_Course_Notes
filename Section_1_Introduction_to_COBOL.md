# Udemy COBOL Course - Section 1: Introduction to COBOL
## 1. Introduction
**COBOL** - Common Business Oriented Language
- made to handle large volumes of data
- general self-documents
- made in 1959

## 3. History and Versions of COBOL
- for course, will be using the current version of COBOL: **Enterprise COBOL for z/os**

## 4. Structure of a COBOL Program

Division > Section > Paragraph > Sentence > Statement > Clause/Verb > Character/Word

### Division Types
**Identification** - name of program (mandatory), programmer-related details, date of program creation, security level

**Environment** - machine-related details (optional)

**Data** - description of data items being processed, define variables. Subdivided into **file**, **working-storage**, and **linkage** sections 

**Procedure** - where the logic of the program is written


- All Divisions and sections must start in AREA A
- can have multiple statements in one sentence over multiple lines with just one period at the end

## 5. Coding Rules in COBOL
- columns 1-6 are reserved for sequence numbers
	- used to label as a source statement line
- column 7 is used for comments, continuations, and debugging mode
	- put `*` to start a comment line
	- put `D` for debugging mode
- columns 8-11 are AREA A
	- for divisions, sections, paragraphs, levels, and declaratives
	- also the end of paragraphs, sections, and divisions always start in AREA A
- columns 12-72 are AREA B
	- contains statements, clauses, words, etc
- columns 73-80 are for program identification and aren't used anymore

## 6. Divisions in COBOL
### Identification Division
- uniquely identifies the name of the program (mandatory)
- optional additional details include: author, installation, date-written, date-compiled, and security label

```
IDENTIFICATION DIVISION.
PROGRAM-ID. <PROGRAMNAME>.
```

### Environmental Division
- contains two sections: **configuration** and **input-output**
- all of this division is optional

**configuration section** - details about the source and/or target computer, details about special characters
**input-output section** - links to external files using **JCL** (Job Control Language)

```
ENVIRONMENT DIVISION.
  CONFIGURATION SECTION.
    SPECIAL-NAMES.
      CURRENCY IS DOLLAR.
      DECIMAL POINT IS COMMA.
```

**Input-Output** Section Syntax:

```
    SELECT <LOCIALFL> ASSIGN TO <PHYSICALFL>
    ORGANIZATION IS <ORG-TYPE>
    ACCESS ODE IS <ACCESS-TYPE>
    FILE STATUS IS <FL-STATUS>.
```

- Only the first line, `SELECT <LOCIALFL> ASSIGN TO <PHYSICALFL>` is mandatory.

```
  INPUT-OUTPUT SECTION.
  FILE CONTROL.
    SELECT <INPUTFL> ASSIGN TO <DDINFL>
    ORGANIZATION IS <SEQUENTIAL>
    ACCESS MODE IS <SEQUENTIAL>
    FILE STATUS IS <INPUTFL-FS>.
```

### Data Division
- used to describe the data items used in the program
- three sections: **file**, **working-storage**, and **linkage**
- there are two additional sections which are rarely used:
	- **report section** - for preparing reports
	- **communication section** - for communicating between two programs simultaneously

**file section** - provides the definition of the files used in program
 - under this is includes the **FD** (File Description) for each file, which defines the layout of the file

```
DATA DIVISION.
  FILE SECTION.
    FD INPUTFL.
    01 INP-RECORD.
      05 INVOICE   PIC X(10).
      05 STORE-ID  PIC X(05).
      05 ITEM-ID   PIC X(10).
```

- the `10` refers to the length of the invoice numbers.

**working-storage section** - used to define the temporary variables used in the program

```
WORKING-STORAGE SECTION.
  01 INPUTFL-FS PIC 9(02).
  01 WS-ID-DISPLAY PIC X(30).
```

**linkage section** - used when a program (main program) calls another (subprogram)
- the linkage section is defined in the subprogram
- you need to define the arguments that are passed by the main program in the linkage section as well

### Procedure Division
- the main division, where business logic is kept
- along with the Identification Division, the other absolutely mandatory division
