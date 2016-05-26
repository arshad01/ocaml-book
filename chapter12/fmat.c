#include "fmat.h"

value array_to_fmat (value faa)
{
    int   i,j;
    value row;

    value fmat = make_fmat(faa);

    float_matrix_t * fm = (float_matrix_t *) Field(fmat,1);

    for (i=0; i<fm->rows; i++)
    {
        row = Field(faa,i);
        for (j=0; j<fm->cols; j++)
        {
            set(fm, i, j, Double_field(row, j));
        }
    } 
    return fmat;
}

value fmat_to_array (value fmat)
{
    value block, row;
    int i, j;

    float_matrix_t * fm = (float_matrix_t *) Field(fmat,1);

    block = make_faa (fm->rows, fm->cols);

    for (i=0; i<fm->rows; i++)
    {
        row = Field(block, i);
        for (j=0; j<fm->cols; j++)
        {
            Store_double_field(row, j, get(fm, i, j));
        }

    }
        
    return block;
}

value fmat_add (value mat1, value mat2)
{
    int i,j;
    value block;
    double add_val;
    float_matrix_t *m1, *m2, *result;

    m1 = (float_matrix_t *)Field(mat1, 1);
    m2 = (float_matrix_t *)Field(mat2, 1);

    if ((m1->rows != m2->rows) && (m1->cols != m2->cols))
        failwith ("fmat_add: Incompatible matrices");

    block = make_fmat2 (m1->rows, m1->cols);
    result = (float_matrix_t *)Field(block, 1);

    for (i=0; i<m1->rows; i++)
    {
        for (j=0; j<m1->cols; j++)
        {
            add_val = get(m1, i, j) + get(m2, i, j);
            set (result, i, j, add_val);
        }
    }

    return block;
}

value fmat_mul (value mat1, value mat2)
{
    int i,j,k;
    value block;
    double part;
    float_matrix_t *m1, *m2, *result;

    m1 = (float_matrix_t *)Field(mat1, 1);
    m2 = (float_matrix_t *)Field(mat2, 1);

    if (m1->cols != m2->rows)
        failwith ("fmat_mul: Incompatible matrices");

    block = make_fmat2 (m1->rows, m2->cols);
    result = (float_matrix_t *)Field(block, 1);

    for (i=0; i<result->rows; i++)
    {
        for (j=0; j<result->cols; j++)
        {
            part = 0.0;
            for (k=0; k<m1->cols; k++)
            {
                part = part + (get(m1, i, k) * get(m2, k, j));
            }
            set(result, i, j, part);
        }
    }

    return block;
}

double get(float_matrix_t* fmat, int r, int c)
{
    double ret;
    int index;

    if ((r > fmat->rows) || (c > fmat->cols))
        failwith ("get: Index out-of-bound");

    index = r*fmat->cols + c;
    ret = fmat->data[index];
    return ret;
}

void set(float_matrix_t* fmat, int r, int c, double val)
{
    int index;

    if ((r > fmat->rows) || (c > fmat->cols))
        failwith ("set: Index out-of-bound");

    index = r*fmat->cols + c;
    fmat->data[index] = val;
    return;
}

value make_fmat (value faa)
{
    value block;
    int rows, cols;

    rows=Wosize_val(faa);
    cols=(rows > 0) ? (Wosize_val(Field(faa,0))) : 0;

    block = make_fmat2 (rows, cols);
    return block;
}

value make_fmat2(int rows, int cols)
{
    value block;
    int   use;
    float_matrix_t *fmat;

    fmat = malloc(sizeof(float_matrix_t));

    if (fmat == NULL)
        failwith ("make_fmat2: Failed to allocate float_matrix");

    fmat->rows = rows;
    fmat->cols = cols;

    fmat->data = (double *)malloc(sizeof(double) * rows * cols);
    if (fmat->data == NULL)
        failwith ("make_fmat2: Failed to allocate float_matrix data");

    use = (sizeof(double)*rows*cols) + sizeof(float_matrix_t);
    block = alloc_final(2, fin_fmat, use, 1000000);
    Field(block,1)=(value)fmat;
    return block;
}

value make_faa(int rows, int cols)
{
    value block;
    int i, num_elems;

    num_elems = cols * Double_wosize;

    block = alloc_tuple(rows);

    for (i=0; i<rows; i++)
    {
        Field(block, i) = alloc (num_elems, Double_array_tag);
    }

    return block;
}

void  fin_fmat (value v)
{
    float_matrix_t *fmat = (float_matrix_t *) Field(v,1);

    free (fmat->data);
    free (fmat);
    //printf("float_matrix gc'd\n");
    //fflush(stdout);
}












