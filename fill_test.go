package fill

import (
	"errors"
	"fmt"
	"go/ast"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
)

func findFuncDeclByFuncName(astFile *ast.File, funcName string) *ast.FuncDecl {
	var funcDecl *ast.FuncDecl
	for _, decl := range astFile.Decls {
		if f, ok := decl.(*ast.FuncDecl); ok {
			if f.Name.Name == funcName {
				funcDecl = f
			}
		}
	}
	return funcDecl
}

func TestGenerateValueWithoutCustomGenerator(t *testing.T) {
	p := NewParser(ParserOptions{})

	wd, _ := os.Getwd()
	err := p.LoadPackage(filepath.Join(wd, "testdata/testfile01.go"))
	if err != nil {
		panic(err)
	}

	funcDecl := findFuncDeclByFuncName(p.AstFile, "Foo")
	if funcDecl == nil {
		panic(errors.New("not found"))
	}

	t.Run("Foo.a", func(t *testing.T) {
		typ := funcDecl.Type.Params.List[0].Type
		valueExpr, lines := p.GenerateValueByAstExpr(typ, nil)
		output, _ := PrepareOutput(valueExpr, lines)
		assert.Equal(t, `""`, output)
	})

	t.Run("Foo.f", func(t *testing.T) {
		typ := funcDecl.Type.Params.List[1].Type
		valueExpr, lines := p.GenerateValueByAstExpr(typ, nil)
		output, _ := PrepareOutput(valueExpr, lines)
		assert.Equal(t, `&F{
	Zoo: zoo.Zoo{
		A:  "",
		AA: "",
		B:  false,
		C:  0,
	},
	zo: &zoo.Zoo{
		A:  "",
		AA: "",
		B:  false,
		C:  0,
	},
	A:  "",
	AA: "",
}`, output)
	})
}

func TestCustomGenerator(t *testing.T) {
	p := NewParser(ParserOptions{
		CustomGenerators: func(p *Parser) {
			p.AddValueGenerator(new(string), func(fi FieldInfo) ast.Expr {
				return &ast.BasicLit{Value: fmt.Sprintf(`"mock_%s"`, fi.FieldName)}
			})
		},
	})

	wd, _ := os.Getwd()
	err := p.LoadPackage(filepath.Join(wd, "testdata/testfile01.go"))
	if err != nil {
		panic(err)
	}
	funcDecl := findFuncDeclByFuncName(p.AstFile, "Foo")
	if funcDecl == nil {
		panic(errors.New("not found"))
	}

	t.Run("Foo.a", func(t *testing.T) {
		param01 := funcDecl.Type.Params.List[0]
		valueExpr, lines := p.GenerateValueByAstExpr(param01.Type, &GenerateOpts{
			FieldName: param01.Names[0].Name,
		})
		output, _ := PrepareOutput(valueExpr, lines)
		assert.Equal(t, `"mock_a"`, output)
	})

	t.Run("Foo.f", func(t *testing.T) {
		param01 := funcDecl.Type.Params.List[1]

		valueExpr0, lines0 := p.GenerateValueByAstExpr(param01.Type, &GenerateOpts{
			FieldName: param01.Names[0].Name,
		})
		output0, _ := PrepareOutput(valueExpr0, lines0)
		assert.Equal(t, `&F{
	Zoo: zoo.Zoo{
		A:  "mock_f",
		AA: "mock_f",
		B:  false,
		C:  0,
	},
	zo: &zoo.Zoo{
		A:  "mock_f",
		AA: "mock_f",
		B:  false,
		C:  0,
	},
	A:  "mock_f",
	AA: "mock_f",
}`, output0)

		valueExpr1, lines1 := p.GenerateValueByAstExpr(param01.Type, &GenerateOpts{
			FieldName:      "value",
			FieldPathsOnly: []string{},
		})
		output1, _ := PrepareOutput(valueExpr1, lines1)
		assert.Equal(t, output1, `&F{}`)

		valueExpr2, lines2 := p.GenerateValueByAstExpr(param01.Type, &GenerateOpts{
			FieldName:      "value",
			FieldPathsOnly: []string{".zo.B", ".A", ".zo.C"},
		})
		output2, _ := PrepareOutput(valueExpr2, lines2)
		assert.Equal(t, `&F{
	Zoo: zoo.Zoo{
		A: "mock_value",
	},
	zo: &zoo.Zoo{
		B: false,
		C: 0,
	},
	A: "mock_value",
}`, output2)
	})
}
