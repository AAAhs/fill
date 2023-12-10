package fill

import (
	"bytes"
	"errors"
	"go/ast"
	"go/format"
	"go/token"
	"path/filepath"

	"golang.org/x/tools/go/packages"
)

func PrepareOutput(n ast.Node, lines int) (string, error) {
	fset := token.NewFileSet()
	file := fset.AddFile("", -1, lines)
	for i := 1; i <= lines; i++ {
		file.AddLine(i)
	}

	var buf bytes.Buffer
	if err := format.Node(&buf, fset, n); err != nil {
		return "", err
	}
	return buf.String(), nil
}

func findFnName(expr ast.Expr) string {
	switch expr := expr.(type) {
	case *ast.Ident:
		return expr.Name
	case *ast.SelectorExpr:
		return expr.Sel.Name
	default:
		return ""
	}
}

func loadPackages(filename string) ([]*packages.Package, error) {
	cfg := &packages.Config{
		Mode:  pkgLoadMode,
		Tests: true,
		Dir:   filepath.Dir(filename),
	}
	pkgs, err := packages.Load(cfg)
	if err != nil {
		return nil, err
	}
	if len(pkgs) == 0 {
		return nil, errors.New("package not found")
	}

	return pkgs, nil
}

func buildImportNameMap(f *ast.File) map[string]string {
	imports := make(map[string]string)
	for _, i := range f.Imports {
		if i.Name != nil && i.Name.Name != "_" {
			importPath := i.Path.Value
			imports[importPath[1:len(importPath)-1]] = i.Name.Name
		}
	}
	return imports
}
