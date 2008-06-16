int bar(void) {
  return sizeof(int);
}

int sum_array(int const* array, int n) {
  int sum = 0;
  int i;
  for(i=0; i<n; i++) {
    sum += array[i];
  }
  return sum;
}

double sum_float_array(float const* array, int n) {
  double sum = 0.0;
  int i;
  for(i=0; i<n; i++) {
    sum += array[i];
  }
  return sum;
}

void square_array_inplace(float* array, int n) {
  int i;
  for(i=0; i<n;i++) {
    array[i] *= array[i];
  }
}

int sum_rev(int *array, int n)
{
  int sum, i;
  for(sum = i = 0; i < n / 2; i++)
  {
    int temp;
    sum += array[i];
    temp = array[i];
    array[i] = array[n - i - 1];
    array[n - i - 1] = temp;
    sum += array[i];
  }

  if(n % 2 != 0) sum += array[n / 2];

  return sum;
}
