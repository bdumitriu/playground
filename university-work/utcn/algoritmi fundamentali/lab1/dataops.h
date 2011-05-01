/*
 * Header for data input/output operations library.
 *
 * Author:  Bogdan DUMITRIU
 * Version: 1.0
 * Date:    27.10.2001
 */

#include <stdio.h>
#include <conio.h>

#ifndef __DATAOPS_H_
#define __DATAOPS_H_

/*
 * This function reads an array from the user, stores it in the
 * _array_ variable and returns its dimension. If an error occurs,
 * -1 is returned. The first element of the array is stored in
 * array[0];
 */
int read_array(int *array);

/*
 * This function reads an array from the user, stores it in the
 * _array_ variable and returns its dimension. If an error occurs,
 * -1 is returned. The first element of the array is stored in
 * array[1];
 */
int read_pascal_array(int *array);

/*
 * This function prints the first _array_length_ elements of the
 * _array_ array. If _stop_for_read_ is 0 than nothing else is
 * done after printing the array. Otherwise, it waits for a
 * character. If an error occurs while printing the data, -1 is
 * returned. On success, it returns 0. The first element of the
 * array is considered to be array[0].
 */
int print_array(int *array, int array_length, int stop_for_read);

/*
 * This function prints the first _array_length_ elements of the
 * _array_ array. If _stop_for_read_ is 0 than nothing else is
 * done after printing the array. Otherwise, it waits for a
 * character. If an error occurs while printing the data, -1 is
 * returned. On success, it returns 0. The first element of the
 * array is considered to be array[1].
 */
int print_pascal_array(int *array, int array_length, int stop_for_read);

#endif