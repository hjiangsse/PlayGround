#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int FirstAccur(int *arr, int t, int p);
int FstBiSearch(int *arr, int t, int l, int u);
int FindFirst(int *arr, int t, int l,int u);
int FindLast(int *arr, int t, int l, int u);

int main(int argc,char **argv)
{
  int arr[7] = {1,3,3,3,4,5,6};
  int f_pos = FindFirst(arr,3,0,6);
  int l_pos = FindLast(arr,3,0,6);
  printf("f_pos: %d\n",f_pos);
  printf("l_pos: %d\n",l_pos);
    return 0;
}

/*
 * Find the first accurence of t in sorted array [arr]
 */

/*
 * Find the first accurence of t in arr before
 * Position p
 */
int FindFirst(int *arr, int t, int l,int u)
{
  assert(arr);
  
  if(l > u) return -1;

  while(l < u)
  {
    int mid = l + (u - l) / 2;

    if((mid == l || arr[mid - 1] < t) && arr[mid] == t)
    {
      return mid;
    }
    
    if(arr[mid] < t)
    {
      l = mid + 1;
    }
    else
    {
      u = mid -1;
    }
  }
}

/*
 * Find the last accurence of t in array[arr]
 */
int FindLast(int *arr, int t, int l, int u)
{
  assert(arr);

  if(l > u) return -1;

  while(l < u)
  {
    int mid = l + (u -l) / 2;

    if((mid == u || arr[mid + 1] > t) && arr[mid] == t)
    {
      return mid;
    }

    if(arr[mid] > t)
    {
      u = mid - 1;
    }
    else
    {
      l = mid + 1;
    }
  }
}

int FirstAccur(int *arr, int t, int p)
{
  assert(arr && p > 0);

  while(arr[p] == t)
  {
    if( p>0 && arr[p-1] == t)
    {
      --p;
    }
    else
    {
      break;
    }
  }

  return p;
}

/*
 * Fist Binary Search
 * find the first accurence of t in arr(l,u)
 * example:
 * arr(1,2,3,3,4,5,6)
 * result: 2
 */
int FstBiSearch(int *arr, int t, int l, int u)
{
  assert(arr);

  if(l > u) return -1;

  while(l < u)
  {
    int mid = l + (u -l) / 2;
    if(arr[mid] == t)
      return FirstAccur(arr,t,mid);
    if(arr[mid] < t)
      l = mid + 1;
    if(arr[mid] > t)
      u = mid -1;
  }
}
