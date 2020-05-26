package benching

import "testing"

func BenchmarkFib20(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib2(20)
	}
}

func BenchmarkFib28(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib2(28)
	}
}

func TestFibIter(t *testing.T) {
	tests := []struct {
		source int
		dest   int
	}{
		{0, 0},
		{1, 1},
		{2, 1},
		{3, 2},
		{4, 3},
		{5, 5},
		{6, 8},
		{7, 13},
	}

	for _, tt := range tests {
		getted := Fib_Iter(tt.source)
		if tt.dest != getted {
			t.Errorf("want %v, but get %v\n", tt.dest, getted)
		}
	}

}

/*
func BenchmarkFib20(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib_Iter(20)
	}
}

func BenchmarkFib28(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib_Iter(28)
	}
}
*/
