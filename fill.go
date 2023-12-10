// Customizing based on github.com/davidrjenni/reftools

package fill

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"runtime"
	"strconv"
	"strings"

	"golang.org/x/tools/go/packages"
)

func init() {
	log.SetFlags(0)
}

type customGeneratorConf struct {
	pkgs    []*packages.Package
	pkg     *packages.Package
	fset    *token.FileSet
	astFile *ast.File
}

type Parser struct {
	cgConf            customGeneratorConf
	valueGeneratorMap map[string]func(fi FieldInfo) ast.Expr

	Pkgs    []*packages.Package
	Pkg     *packages.Package
	Fset    *token.FileSet
	AstFile *ast.File
}

type FieldInfo struct {
	FieldDefPath []*types.Var
	FieldName    string
}

type ParserOptions struct {
	CustomGenerators func(p *Parser)
}

func NewParser(opts ParserOptions) *Parser {
	p := &Parser{}

	if opts.CustomGenerators != nil {
		_, filename, _, _ := runtime.Caller(1)
		err := p.loadCustomGeneratorPackage(filename)
		if err != nil {
			panic(err)
		}
		opts.CustomGenerators(p)
	}

	return p
}

func (p *Parser) loadCustomGeneratorPackage(filename string) error {
	pkgs, err := loadPackages(filename)
	if err != nil {
		return err
	}

	p.cgConf.pkgs = pkgs
	p.cgConf.fset = pkgs[0].Fset

	for _, pkg := range pkgs {
		for _, af := range pkg.Syntax {
			if file := pkg.Fset.File(af.Pos()); file.Name() == filename {
				p.cgConf.astFile = af
				p.cgConf.pkg = pkg
			}
		}
	}

	p.valueGeneratorMap = make(map[string]func(fi FieldInfo) ast.Expr)

	return nil
}

type TypePointer any

func (p *Parser) AddValueGenerator(t TypePointer, fn func(fi FieldInfo) ast.Expr) {
	// to prevent `unused parameter` lint
	_ = t

	// get caller line
	_, _, line, _ := runtime.Caller(1)

	fset := p.cgConf.fset
	astFile := p.cgConf.astFile
	pkg := p.cgConf.pkg

	var expr ast.Expr
	ast.Inspect(astFile, func(node ast.Node) bool {
		if callExpr, ok := node.(*ast.CallExpr); ok {
			if findFnName(callExpr.Fun) == "AddValueGenerator" && fset.Position(callExpr.Pos()).Line == line {
				if len(callExpr.Args) == 0 {
					return true
				}

				callNewExpr, ok := callExpr.Args[0].(*ast.CallExpr)
				if !ok || findFnName(callNewExpr.Fun) != "new" {
					return true
				}

				if len(callNewExpr.Args) == 0 {
					return true
				}

				expr = callNewExpr.Args[0]
			}
		}
		return true
	})
	if expr == nil {
		log.Println("type not found")
		return
	}

	typeAndValue, has := pkg.TypesInfo.Types[expr]
	if !has {
		log.Println("type not found")
		return
	}

	typ := typeAndValue.Type
	name, _ := typ.(*types.Named)

	var id string
	// TODO:
	if name != nil {
		id = name.Obj().Id()
	} else {
		id = typ.String()
	}

	p.valueGeneratorMap[id] = fn
}

func (p *Parser) LoadPackage(filename string) error {
	pkgs, err := loadPackages(filename)
	if err != nil {
		return err
	}

	p.Pkgs = pkgs
	p.Fset = pkgs[0].Fset
	for _, pkg := range pkgs {
		for _, af := range pkg.Syntax {
			if file := pkg.Fset.File(af.Pos()); file.Name() == filename {
				p.AstFile = af
				p.Pkg = pkg
			}
		}
	}

	return nil
}

type litInfo struct {
	typ       types.Type   // the base type of the literal
	name      *types.Named // name of the type or nil, e.g. for an anonymous struct type
	hideType  bool         // flag to hide the element type inside an array, slice or map literal
	isPointer bool         // true if the literal is of a pointer type
}

type filler struct {
	parser         *Parser
	pkg            *packages.Package
	pos            token.Pos
	lines          int
	existing       map[string]*ast.KeyValueExpr
	first          bool
	importNames    map[string]string // import path -> import name
	fieldPathsOnly []string
	fieldName      string
}

type GenerateOpts struct {
	FieldPathsOnly []string
	FieldName      string
}

func (p *Parser) GenerateValueByAstExpr(typeExpr ast.Expr, opts *GenerateOpts) (ast.Expr, int) {
	typeAndValue, ok := p.Pkg.TypesInfo.Types[typeExpr]
	if !ok {
		return nil, 0
	}

	typ := typeAndValue.Type
	name, _ := typ.(*types.Named)

	litInfo := litInfo{
		typ:  typ,
		name: name,
	}

	importNames := buildImportNameMap(p.Pkg.Syntax[0])

	f := &filler{
		parser:      p,
		pkg:         p.Pkg,
		pos:         1,
		first:       true,
		existing:    make(map[string]*ast.KeyValueExpr),
		importNames: importNames,
	}
	if opts != nil {
		f.fieldName = opts.FieldName
		f.fieldPathsOnly = opts.FieldPathsOnly
	}

	return f.fillValue(litInfo, make([]types.Type, 0, 8), nil), f.lines
}

func (p *Parser) GenerateValueByTypesType(typ types.Type, opts *GenerateOpts) (ast.Expr, int) {
	name, _ := typ.(*types.Named)

	litInfo := litInfo{
		typ:  typ,
		name: name,
	}

	importNames := buildImportNameMap(p.Pkg.Syntax[0])

	f := &filler{
		parser:      p,
		pkg:         p.Pkg,
		pos:         1,
		first:       true,
		existing:    make(map[string]*ast.KeyValueExpr),
		importNames: importNames,
	}
	if opts != nil {
		f.fieldName = opts.FieldName
		f.fieldPathsOnly = opts.FieldPathsOnly
	}

	return f.fillValue(litInfo, make([]types.Type, 0, 8), nil), f.lines
}

func (f *filler) fillValue(info litInfo, visited []types.Type, fieldDefPath []*types.Var) ast.Expr {
	// try to use custom value generator
	var id string
	if info.name != nil {
		id = info.name.Obj().Id()
	} else {
		id = info.typ.String()
	}
	fn, has := f.parser.valueGeneratorMap[id]
	if has {
		return fn(FieldInfo{
			FieldDefPath: fieldDefPath,
			FieldName:    f.fieldName,
		})
	}

	switch t := info.typ.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.Bool:
			return &ast.Ident{Name: "false", NamePos: f.pos}
		case types.Int, types.Int8, types.Int16, types.Int32, types.Int64:
			return &ast.BasicLit{Value: "0", ValuePos: f.pos}
		case types.Uint, types.Uint8, types.Uint16, types.Uint32, types.Uint64:
			return &ast.BasicLit{Value: "0", ValuePos: f.pos}
		case types.Uintptr:
			return &ast.BasicLit{Value: "uintptr(0)", ValuePos: f.pos}
		case types.UnsafePointer:
			return &ast.BasicLit{Value: "unsafe.Pointer(uintptr(0))", ValuePos: f.pos}
		case types.Float32, types.Float64:
			return &ast.BasicLit{Value: "0.0", ValuePos: f.pos}
		case types.Complex64, types.Complex128:
			return &ast.BasicLit{Value: "(0 + 0i)", ValuePos: f.pos}
		case types.String:
			return &ast.BasicLit{Value: `""`, ValuePos: f.pos}
		default:
			// Cannot create an expression for an invalid type.
			return nil
		}
	case *types.Chan:
		valTypeName, ok := typeString(f.pkg.Types, f.importNames, t.Elem())
		if !ok {
			return nil
		}

		var dir ast.ChanDir
		switch t.Dir() {
		case types.SendRecv:
			dir = ast.SEND | ast.RECV
		case types.SendOnly:
			dir = ast.SEND
		case types.RecvOnly:
			dir = ast.RECV
		}

		return &ast.CallExpr{
			Fun: &ast.Ident{
				NamePos: f.pos,
				Name:    "make",
			},
			Lparen: f.pos,
			Args: []ast.Expr{
				&ast.ChanType{
					Dir:   dir,
					Value: ast.NewIdent(valTypeName),
				},
			},
			Rparen: f.pos,
		}
	case *types.Interface:
		return &ast.Ident{Name: "nil", NamePos: f.pos}
	case *types.Map:
		keyTypeName, ok := typeString(f.pkg.Types, f.importNames, t.Key())
		if !ok {
			return nil
		}
		valTypeName, ok := typeString(f.pkg.Types, f.importNames, t.Elem())
		if !ok {
			return nil
		}
		lit := &ast.CompositeLit{
			Lbrace: f.pos,
			Type: &ast.MapType{
				Map:   f.pos,
				Key:   ast.NewIdent(keyTypeName),
				Value: ast.NewIdent(valTypeName),
			},
		}
		f.pos++
		lit.Elts = []ast.Expr{
			&ast.KeyValueExpr{
				Key:   f.fillValue(litInfo{typ: t.Key(), name: info.name, hideType: true}, visited, fieldDefPath),
				Colon: f.pos,
				Value: f.fillValue(litInfo{typ: t.Elem(), name: info.name, hideType: true}, visited, fieldDefPath),
			},
		}
		f.pos++
		lit.Rbrace = f.pos
		f.lines += 2
		return lit
	case *types.Signature:
		params := make([]*ast.Field, t.Params().Len())
		for i := 0; i < t.Params().Len(); i++ {
			typeName, ok := typeString(f.pkg.Types, f.importNames, t.Params().At(i).Type())
			if !ok {
				return nil
			}
			params[i] = &ast.Field{
				Type: ast.NewIdent(typeName),
			}
		}
		results := make([]*ast.Field, t.Results().Len())
		for i := 0; i < t.Results().Len(); i++ {
			typeName, ok := typeString(f.pkg.Types, f.importNames, t.Results().At(i).Type())
			if !ok {
				return nil
			}
			results[i] = &ast.Field{
				Type: ast.NewIdent(typeName),
			}
		}
		return &ast.FuncLit{
			Type: &ast.FuncType{
				Func:    f.pos,
				Params:  &ast.FieldList{List: params},
				Results: &ast.FieldList{List: results},
			},
			Body: &ast.BlockStmt{
				List: []ast.Stmt{
					&ast.ExprStmt{X: ast.NewIdent(`panic("not implemented")`)},
				},
			},
		}
	case *types.Slice:
		return f.fillSequence(info, visited, t, nil)

	case *types.Array:
		return f.fillSequence(info, visited, t, &ast.BasicLit{Value: strconv.FormatInt(t.Len(), 10)})

	case *types.Named:
		if _, ok := t.Underlying().(*types.Struct); ok {
			info.name = t
		}
		info.typ = t.Underlying()
		return f.fillValue(info, visited, fieldDefPath)

	case *types.Pointer:
		if _, ok := t.Elem().Underlying().(*types.Struct); ok {
			info.typ = t.Elem()
			info.isPointer = true
			return f.fillValue(info, visited, fieldDefPath)
		}
		return &ast.Ident{Name: "nil", NamePos: f.pos}

	case *types.Struct:
		newlit := &ast.CompositeLit{Lbrace: f.pos}
		if !info.hideType && info.name != nil {
			typeName, ok := typeString(f.pkg.Types, f.importNames, info.name)
			if !ok {
				return nil
			}
			newlit.Type = ast.NewIdent(typeName)
			if info.isPointer {
				newlit.Type.(*ast.Ident).Name = "&" + newlit.Type.(*ast.Ident).Name
			}
		} else if !info.hideType && info.name == nil {
			typeName, ok := typeString(f.pkg.Types, f.importNames, t)
			if !ok {
				return nil
			}
			newlit.Type = ast.NewIdent(typeName)
		}

		for _, typ := range visited {
			if t == typ {
				return newlit
			}
		}
		visited = append(visited, t)

		first := f.first
		f.first = false
		lines := 0
		imported := isImported(f.pkg.Types, info.name)

		for i := 0; i < t.NumFields(); i++ {
			field := t.Field(i)

			newFieldDefPath := append(fieldDefPath, field)
			var defPath string
			var accessiblePath string
			for _, f := range newFieldDefPath {
				defPath += "." + f.Name()
				if !f.Embedded() {
					accessiblePath += "." + f.Name()
				}
			}

			if f.fieldPathsOnly != nil {
				fieldPathPrefixExists := false
				for _, fp := range f.fieldPathsOnly {
					if strings.HasPrefix(fp, accessiblePath) ||
						strings.HasPrefix(fp, defPath) {
						fieldPathPrefixExists = true
					}
				}
				if !fieldPathPrefixExists {
					continue
				}
			}

			if kv, ok := f.existing[field.Name()]; first && ok {
				f.pos++
				lines++
				f.fixExprPos(kv)
				newlit.Elts = append(newlit.Elts, kv)
			} else if !ok && !imported || field.Exported() {
				f.pos++
				k := &ast.Ident{Name: field.Name(), NamePos: f.pos}
				if v := f.fillValue(litInfo{typ: field.Type(), name: nil}, visited, newFieldDefPath); v != nil {
					lines++
					newlit.Elts = append(newlit.Elts, &ast.KeyValueExpr{
						Key:   k,
						Value: v,
					})
				} else {
					f.pos--
				}
			}
		}
		if lines > 0 {
			f.lines += lines + 2
			f.pos++
		}
		newlit.Rbrace = f.pos
		return newlit

	default:
		panic(fmt.Sprintf("unexpected type %T", t))
	}
}

// sequence is a interface that abstracts
// between *types.Slice and *types.Array
type sequence interface {
	Elem() types.Type
}

func (f *filler) fillSequence(info litInfo, visited []types.Type, t sequence, length ast.Expr) ast.Expr {
	lit := &ast.CompositeLit{Lbrace: f.pos}
	if !info.hideType {
		typeName, ok := typeString(f.pkg.Types, f.importNames, t.Elem())
		if !ok {
			return nil
		}
		lit.Type = &ast.ArrayType{
			Lbrack: f.pos,
			Len:    length,
			Elt:    ast.NewIdent(typeName),
		}
	}
	if arr, isArray := t.(*types.Array); isArray {
		lit.Elts = make([]ast.Expr, 0, arr.Len())
		for i := int64(0); i < arr.Len(); i++ {
			f.pos++
			elemInfo := litInfo{typ: t.Elem().Underlying(), hideType: true}
			elemInfo.name, _ = t.Elem().(*types.Named)
			if v := f.fillValue(elemInfo, visited, nil); v != nil {
				lit.Elts = append(lit.Elts, v)
			}
		}
		f.lines += len(lit.Elts)
	}
	f.lines += 2
	f.pos++
	lit.Rbrace = f.pos
	return lit
}

func (f *filler) fixExprPos(expr ast.Expr) {
	switch expr := expr.(type) {
	case nil:
		// ignore
	case *ast.BasicLit:
		expr.ValuePos = f.pos
	case *ast.BinaryExpr:
		f.fixExprPos(expr.X)
		expr.OpPos = f.pos
		f.fixExprPos(expr.Y)
	case *ast.CallExpr:
		f.fixExprPos(expr.Fun)
		expr.Lparen = f.pos
		for _, arg := range expr.Args {
			f.fixExprPos(arg)
		}
		expr.Rparen = f.pos
	case *ast.CompositeLit:
		f.fixExprPos(expr.Type)
		expr.Lbrace = f.pos
		for _, e := range expr.Elts {
			f.pos++
			f.fixExprPos(e)
		}
		if l := len(expr.Elts); l > 0 {
			f.lines += l + 2
		}
		f.pos++
		expr.Rbrace = f.pos
	case *ast.Ellipsis:
		expr.Ellipsis = f.pos
	case *ast.FuncLit:
		expr.Type.Func = f.pos
	case *ast.Ident:
		expr.NamePos = f.pos
	case *ast.IndexExpr:
		f.fixExprPos(expr.X)
		expr.Lbrack = f.pos
		f.fixExprPos(expr.Index)
		expr.Rbrack = f.pos
	case *ast.KeyValueExpr:
		f.fixExprPos(expr.Key)
		f.fixExprPos(expr.Value)
	case *ast.ParenExpr:
		expr.Lparen = f.pos
	case *ast.SelectorExpr:
		f.fixExprPos(expr.X)
		expr.Sel.NamePos = f.pos
	case *ast.SliceExpr:
		f.fixExprPos(expr.X)
		expr.Lbrack = f.pos
		f.fixExprPos(expr.Low)
		f.fixExprPos(expr.High)
		f.fixExprPos(expr.Max)
		expr.Rbrack = f.pos
	case *ast.StarExpr:
		expr.Star = f.pos
		f.fixExprPos(expr.X)
	case *ast.UnaryExpr:
		expr.OpPos = f.pos
		f.fixExprPos(expr.X)
	}
}

func isImported(pkg *types.Package, n *types.Named) bool {
	return n != nil && pkg != n.Obj().Pkg()
}
