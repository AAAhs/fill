package testdata

import "github.com/AAAhs/fill/testdata/zoo"

type F struct {
	zoo.Zoo
	zo *zoo.Zoo
	A  string
	AA string
}

func Foo(
	a string,
	f *F,
) string {
	return a + f.A
}
