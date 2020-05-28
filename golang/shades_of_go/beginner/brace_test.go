package main

import "testing"

func BenchmarkGetValueByString(b *testing.B) {
	table := map[string][]byte{"abcdefghijklmn": []byte("hello")}
	var key string = "abcdefghijklmn"
	b.ResetTimer()
	for n := 0; n <= b.N; n++ {
		GetValueByString(table, key)
	}
}

func BenchmarkGetValueBySlice(b *testing.B) {
	table := map[string][]byte{"abcdefghijklmn": []byte("hello")}
	var key []byte = []byte("abcdefghijklmn")
	b.ResetTimer()
	for n := 0; n <= b.N; n++ {
		GetValueBySlice(table, key)
	}
}
