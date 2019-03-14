#ifndef _BIT_VEC_H_
#define _BIT_VEC_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define ELEM_LEN  sizeof(int)
#define ELEM_BITS (sizeof(int) * 8)

typedef struct _BitVec
{
    int veclen; //the number of usefullness elements in bitvector
    int size;   //the true size of the bitvec (base elment is int)
    int *pElms;  //the head address of the bit vector
}BitVec, *pBitVec;

typedef struct _SlotBitVec
{
  int slotsize; //the number of bits in a slot
  int veclen;   //the valid lenth of the vector
  int size;     //the true size of the bitvec
  int *pElms;
}SlotBitVec, *pSlotBitVec;

/*-------------------------------------------------------------------*/
//create a bitvector which have "veclen" element, then initialize it
void CreateVec(int veclen, BitVec *pVec);
//reinitialize a bitvector
void ClearVec(BitVec *pVec);
//free the space allocated by the bitvector
void FreeVec(BitVec *pVec);
//set element in pos, paramenter val is 0 or 1
void SetBit(BitVec *pVec, int pos, int val);
//get element in pos
int GetBit(BitVec *pVec, int pos);
//print a bit vector
void print_vec(BitVec *pVec);
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
int CreateSlotVec(int slotsize, int veclen, BitVec *pVec);
void ClearSlotVec(SlotBitVec *pVec);
void FreeSlotVec(SlotBitVec *pVec);
int IncSlot(SlotBitVec *pVec, int pos);
*-------------------------------------------------------------------*/

#endif
