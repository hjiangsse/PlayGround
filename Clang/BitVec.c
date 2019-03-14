#include "BitVec.h"

#define MAX_VECSIZE 10000000

static int valid_slot(int slotsize);

void CreateVec(int veclen, BitVec *pVec)
{
    assert(veclen >= 0 && pVec);

    if (veclen == 0)
    {
        pVec->veclen = 0;
        pVec->size = 0;
        pVec->pElms = NULL;
    }
    else
    {
        pVec->veclen = veclen;
        pVec->size = (veclen % ELEM_BITS) == 0 ? (veclen / ELEM_BITS) : ((veclen / ELEM_BITS) + 1);
        pVec->pElms = (int *)malloc(pVec->size * ELEM_LEN);
        if (pVec->pElms)
        {
            //initialize all bits to '0'
            memset(pVec->pElms, 0x00, pVec->size * ELEM_LEN);
        }
    }

    return;
}

void FreeVec(BitVec *pVec)
{
    assert(pVec);

    if (pVec->pElms != NULL && pVec->veclen > 0)
    {
        free(pVec->pElms);
        pVec->veclen = 0;
        pVec->size = 0;
    }
}

void SetBit(BitVec *pVec, int pos, int val)
{
    assert(pVec &&
           pos >= 0 &&
           pos < pVec->veclen &&
           (val == 0 || val == 1));

    //get the element position
    int *elem_pos = pVec->pElms + ( pos / ELEM_BITS );
    int mask = 0x01 << (pos % ELEM_BITS);
    if(val == 0)
    {
        *elem_pos &= ~mask;
    }
    else
    {
        *elem_pos |= mask;
    }
}

int GetBit(BitVec *pVec, int pos)
{
    assert(pVec &&
           pos >= 0 &&
           pos < pVec->veclen);

    //get the temp int slot
    int tmp = pVec->pElms[pos/ELEM_BITS];
    tmp = tmp >> (pos % ELEM_BITS);
    tmp = tmp & 0x01;

    return tmp;
}

void ClearVec(BitVec *pVec)
{
    assert(pVec);

    memset(pVec->pElms, 0x00, pVec->size * ELEM_LEN);
}

void print_vec(BitVec *pVec)
{
    assert(pVec);

    int i;
    printf("---------Msg of BitVec Begin------------\n");
    printf("VecLen: %d\n", pVec->veclen);
    printf("VecSize: %d\n", pVec->size);
    printf("VecElems: \n");
    for(i = 0; i < pVec->veclen; i++)
    {
        printf("%d", GetBit(pVec, i));
    }
    printf("\n");
    printf("---------Msg of BitVec End--------------\n");
}

static int valid_slot(int slotsize)
{
  int tmp = 1;
  while (tmp < slotsize)
  {
    tmp = tmp << 1;
  }

  if(tmp == slotsize)
  {
    return 0;
  }

  return -1;
}

int CreateSlotVec(int slotsize, int veclen, BitVec *pVec)
{
    assert(slotsize > 0 && veclen > 0 && pVec);

    if(valid_slot(slotsize) != 0)
    {
        fprintf(stderr,"[Error] InValid Slot Size: %d\n", slotsize);
        return -1;
    }

    int bitsize = slotsize * veclen;

    pVec->slotsize = slotsize;
    pVec->veclen = veclen;
    pVec->size = (bitsize) % ELEM_BITS == 0 ?
      (bitsize / ELEM_BITS) : ((bitsize / ELEM_BITS) + 1);
    pVec->pElms = (int *)malloc(ELEM_LEN * pVec->size);

    if(pVec->pElms)
    {
        memset(pVec->pElms, 0x00, ELEM_LEN * pVec->size);
    }
    else
    {
        return -1;
    }

    return 0;
}

void ClearSlotVec(SlotBitVec *pVec)
{
    assert(pVec);

    memset(pVec->pElms, 0x00, ELEM_LEN * pVec->size);
}

void FreeSlotVec(SlotBitVec *pVec)
{
    assert(pVec);

    if(pVec->pElms != NULL && pVec->size > 0)
    {
        free(pVec->pElms);
        pVec->slotsize = 0;
        pVec->veclen = 0;
        pVec->size = 0;
    }
}

int IncSlot(SlotBitVec *pVec, int pos)
{
    assert(pVec && pos >= 0);

    if(pos >= pVec->veclen)
    {
      fprintf(stderr, "[Error] Wrong position: %d, out of scope!\n", pos);
      return -1;
    }

    //get the int index which "pos" is in
    int ipos = pos / (ELEM_BITS / pVec->slotsize);
    //get the "pos" in specific slot
    int islot = pos % (ELEM_BITS / pVec->slotsize);

    //shift distance
    int shift_dis = (islot - 1) * pVec->slotsize;

    //do inc
    int *elePos = pVec->pElms + ipos;
    *elsPos += (1 << shift_dis);

    return 0;
}

int main(int argc, char *argv[])
{
  int slotsize = 15;
  if(valid_slot(slotsize) == 0)
  {
    printf("%d is a valid size!\n", slotsize);
  }
  else
  {
    printf("%d is a invalid size!\n", slotsize);
  }
  return 0;
}
