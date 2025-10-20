# COBOL Snippets & Stuff

A few days ago I thought to myself "Hey, why don't you start studying a language that is at least a decade older than your fucking parents?". Here we are I guess, what could possibly go wrong?

## Index

- [Basics](#basics)
- - [Numeric types](#numeric-types)
- - [Alphabetic/Alphanumeric types](#alphabeticalphanumeric-types)
- - [Boolean types](#boolean-types)
- - [Special types](#special-types)
- [Level numbers](#level-numbers)

## Basics

Cobol has a rigid structure, it's divided in **DIVISIONS** such as:

- IDENTIFICATION DIVISION (Program metadata)
- ENVIRNONMENT DIVISION (Info about the system and used files)
- DATA DIVISIONS (Variables are alvays defined here)
- PROCEDURE DIVISION (Actual code for logic)

There are no classic data types as intended in C, Java or Python
COBOL uses the **PICTURE** clause (plus a couple others) to define format, size and usage of the data.

we can divide the main types in:

- Numeric
- Alphabetic/Alphanumeric
- Boolean
- Special types

### Numeric types

Numeric types are defined using the `9` character in the PICTURE clause.

| Type                          | Description                            | Example                                                  |
| ----------------------------- | -------------------------------------- | -------------------------------------------------------- |
| **Numeric (display)**         | Standard decimal digits stored as text | `01 NUM PIC 9(5).` → 5-digit number                      |
| **Signed numeric**            | Can store positive or negative numbers | `01 NUM PIC S9(5).`                                      |
| **Decimal (implied decimal)** | `V` indicates the decimal position     | `01 NUM PIC 9(3)V99.` → 3 digits before, 2 after decimal |
| **Packed decimal (COMP-3)**   | Binary-coded decimal, space-efficient  | `01 NUM PIC S9(5)V99 COMP-3.`                            |
| **Binary (COMP)**             | Binary storage                         | `01 NUM PIC S9(5) COMP.`                                 |
| **Floating point (COMP)**     | Usually 32-bit or 64-bit floating      | `01 NUM USAGE COMP` with decimals                        |
| **Zoned decimal**             | Each digit stored in a byte with sign  | `01 NUM PIC S9(5) DISPLAY.`                              |

#### Alphabetic/Alphanumeric types

Alphabetic and Alphanumeric types are defined using the `A` and `X` characters in the PICTURE clause.

| Type                    | PIC                      | Description                           | Example              |
| ----------------------- | ------------------------ | ------------------------------------- | -------------------- |
| **Alphabetic**          | `A`                      | Letters only (A–Z)                    | `01 NAME PIC A(10).` |
| **Alphanumeric**        | `X`                      | Any character                         | `01 STR PIC X(20).`  |
| **National (Unicode)**  | `N`                      | For Unicode/UTF-16                    | `01 STR PIC N(10).`  |
| **Alphanumeric edited** | `X` with editing symbols | Formatted text                        | `01 STR PIC XXX-XX.` |

### Boolean types

COBOL didn't originally have a dedicated boolean type. Instead, booleans are typically represented using numeric or alphanumeric types.

| Type                      | Description                           | Example                          |
| ------------------------- | ------------------------------------- | -------------------------------- |
| **Boolean (numeric)**     | 0 = false, 1 = true                   | `01 FLAG PIC 9.`                 |
| **Boolean (alphanumeric)**| 'N' = false, 'Y' = true               | `01 FLAG PIC X(1).`              |

Newer COBOL standards may include a BOOLEAN type, but its support can vary by compiler.

```cobol
01 FLAG BOOLEAN VALUE TRUE.
```

### Special types

Special types in COBOL include various usages for specific storage or formatting needs.

| Type                  | Usage                                                    |
| --------------------- | -------------------------------------------------------- |
| **Date / Time**       | Can use `PIC 9(8)` or libraries for date (`YYYYMMDD`)    |
| **Pointer / Address** | `USAGE POINTER`                                          |
| **Procedure pointer** | `USAGE PROCEDURE-POINTER`                                |
| **Function result**   | Temporary item used in function calls                    |
| **Group items**       | 01-level or higher items without PIC, grouping subfields |

Example of a group item:

```cobol
01 EMPLOYEE.
   05 EMP-NAME PIC X(20).
   05 EMP-ID   PIC 9(5).
   05 EMP-SAL  PIC 9(6)V99.
```

## Level numbers

Level numbers in COBOL define the hierarchy and structure of data items within the DATA DIVISION. They indicate the relationship between different data items, allowing for the organization of complex data structures.

| Level     | Meaning                                                                         |
| --------- | ------------------------------------------------------------------------------- |
| **01**    | Top-level item (a “group” or “elementary” item).                                |
| **02–49** | Sub-items inside a group. Each level is nested under the previous higher level. |
| **66**    | **RENAMES** clause – gives alternative names to an existing group of fields.    |
| **77**    | Standalone elementary item, not part of a group. Can also use for constants.    |
| **88**    | **Condition-name** – special Boolean-like flag associated with a data item.     |

Example of standalone item:

```cobol
77 MAX-SALARY PIC 9(6)V99 VALUE 999999.99.
```

Example of condition-name:

```cobol
01 EMP-STATUS     PIC X.
   88 ACTIVE      VALUE 'A'.
   88 INACTIVE    VALUE 'I'.
```

That could be used like:

```cobol
IF EMP-STATUS = ACTIVE
   DISPLAY "Employee active".
ELSE
   DISPLAY "Employee inactive".
ENDIF.
```

Making use of condition-names improves code readability significantly.
