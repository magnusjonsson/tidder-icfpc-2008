int bar() {
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
