#include <stdio.h>

extern int fact(int n);

int main(void){
	int nn;
	scanf("%d", &nn);
	printf("Policzmy n!: %d\n", fact(nn));
	return 0;
}
