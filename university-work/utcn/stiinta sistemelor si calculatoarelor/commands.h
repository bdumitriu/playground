/*
 * Author: Bogdan DUMITRIU
 * Date:   04.2002
 *
 * This is the header file from the library which contains examples
 * of using the DOS functions.
 */

#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <stdlib.h>

#ifndef __COMMANDS_H_
#define __COMMANDS_H_

// functions for submenu 1
void fn01h();
void fn08h();
void fn07h();
void fn06h();
void fn0bh();
void fn0ch();
void fn0ah();
void fn02h();
void fn09h();
void fn05h();

// functions for submenu 2
void fn4406h();
void fn4407h();
void fn4408h();

// functions for submenu 3
void fn39h();
void fn3ah();
void fn3ch();
void fn41h();
void fn56h();
void fn4300h();
void fn4301h();

// functions for submenu 4
void fn440c5fh();
void fn440c7fh();

// functions for submenu 5
void fn19h();
void fn0eh();
void fn47h();
void fn3bh();
void fn36h();
void fn3305h();

// functions for submenu 6
void fn3dh();
void fn3fh();
void fn40h();

// functions for submenu 7
void fn30h();
void fn2ah();
void fn2ch();
void fn35h();

#endif