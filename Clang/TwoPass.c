#include "BitVec.h"

#define MAX_VECSIZE 8000000
#define MAX_NUM     10000000

/*
 * We just have 1MB memory, but we have 1000w uniq
 * positive numbers(all less than 1000w) to be sorted
 * so we use a two pass method
 */

int main(int argc, char *argv[])
{
    BitVec bv = {0};
    FILE *fp;
    FILE *op;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int i;

    CreateVec(MAX_VECSIZE, &bv);

    if(argc != 3)
    {
        printf("Usage: BitVec filename sorted_file\n");
    }
    else
    {
        fp = fopen(argv[1], "r");
        op = fopen(argv[2], "a+");

        if(fp == NULL) exit(1);

        // first travese, sort number < 8000000
        while((read = getline(&line, &len, fp)) != -1)
        {
            int inum = atoi(line);
            if (inum < MAX_VECSIZE)
            {
                SetBit(&bv, inum, 1);
            }
        }

        for(i = 0; i < MAX_VECSIZE; i++)
        {
            if(GetBit(&bv, i))
            {
              fprintf(op, "%d\n", i);
            }
        }

        ClearVec(&bv);

        // second travese, sort number > 8000000
        if(!fseek(fp, 0, SEEK_SET))
        {
            while ((read = getline(&line, &len, fp)) != -1)
            {
                int inum = atoi(line);
                if(inum >= MAX_VECSIZE)
                {
                    SetBit(&bv, inum - MAX_VECSIZE, 1);
                }
            }

            for(i = 0; i < MAX_NUM - MAX_VECSIZE; i++)
            {
                if(GetBit(&bv, i))
                {
                    fprintf(op, "%d\n", i + MAX_VECSIZE);
                }
            }
        }

        fclose(fp);
        fclose(op);
    }

    FreeVec(&bv);
    return 0;
}
