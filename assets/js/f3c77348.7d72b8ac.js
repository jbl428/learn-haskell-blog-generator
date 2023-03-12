"use strict";(self.webpackChunklearn_haskell_blog_generator=self.webpackChunklearn_haskell_blog_generator||[]).push([[5615],{3905:(e,t,a)=>{a.d(t,{Zo:()=>u,kt:()=>h});var n=a(7294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},l=Object.keys(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=n.createContext({}),p=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},u=function(e){var t=p(e.components);return n.createElement(s.Provider,{value:t},e.children)},d="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},c=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,l=e.originalType,s=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),d=p(a),c=r,h=d["".concat(s,".").concat(c)]||d[c]||m[c]||l;return a?n.createElement(h,i(i({ref:t},u),{},{components:a})):n.createElement(h,i({ref:t},u))}));function h(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=a.length,i=new Array(l);i[0]=c;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o[d]="string"==typeof e?e:r,i[1]=o;for(var p=2;p<l;p++)i[p]=a[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}c.displayName="MDXCreateElement"},6530:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>m,frontMatter:()=>l,metadata:()=>o,toc:()=>p});var n=a(7462),r=(a(7294),a(3905));const l={},i="Representing the markup language as a Haskell data type",o={unversionedId:"markup/data_type",id:"markup/data_type",title:"Representing the markup language as a Haskell data type",description:"One of the clear differentiators between Haskell (also other ML-family of languages)",source:"@site/docs/04-markup/01-data_type.md",sourceDirName:"04-markup",slug:"/markup/data_type",permalink:"/learn-haskell-blog-generator/docs/markup/data_type",draft:!1,editUrl:"https://github.com/jbl428/learn-haskell-blog-generator/tree/book/docs/04-markup/01-data_type.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"HTML \ucd9c\ub825 \ub77c\uc774\ube0c\ub7ec\ub9ac \ub9cc\ub4e4\uae30",permalink:"/learn-haskell-blog-generator/docs/html_printer"},next:{title:"Parsing markup part 01 (Recursion)",permalink:"/learn-haskell-blog-generator/docs/markup/parsing_01"}},s={},p=[{value:"Exercises",id:"exercises",level:3},{value:"Translating directly?",id:"translating-directly",level:2}],u={toc:p},d="wrapper";function m(e){let{components:t,...a}=e;return(0,r.kt)(d,(0,n.Z)({},u,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"representing-the-markup-language-as-a-haskell-data-type"},"Representing the markup language as a Haskell data type"),(0,r.kt)("p",null,"One of the clear differentiators between Haskell (also other ML-family of languages)\nand most mainstream languages is the ability to represent data precisely and succinctly."),(0,r.kt)("p",null,"So how do we represent our markup language using Haskell?"),(0,r.kt)("p",null,"Previously, in our HTML builder library, we used ",(0,r.kt)("inlineCode",{parentName:"p"},"newtype"),"s to differentiate\nbetween HTML documents, structures and titles, but we didn't really need to\ndifferentiate between different kinds of structures such as paragraphs and headings,\nnot without parsing the data at least."),(0,r.kt)("p",null,"In this case, we have a list of structures, and each structure could be\none of a few specific options (a paragraph, a heading, a list, etc.),\nand we want to be able to know which structure is which so we can easily\nconvert it into the equivalent HTML representation."),(0,r.kt)("p",null,"For that, we have ",(0,r.kt)("inlineCode",{parentName:"p"},"data")," definitions. ",(0,r.kt)("inlineCode",{parentName:"p"},"data")," gives us the ability to\ncreate custom types by grouping multiple types together and having\nalternative structures. Think of them as combination of both structs and enums."),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"data")," declarations look like this:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data <Type-name> <type-args>\n  = <Data-constructor1> <types>\n  | <Data-constructor2> <types>\n  | ...\n")),(0,r.kt)("p",null,"It looks really similar to ",(0,r.kt)("inlineCode",{parentName:"p"},"newtype"),", but there are two important\ndifferences:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"In the ",(0,r.kt)("inlineCode",{parentName:"li"},"<types>")," part we can write many types (Like ",(0,r.kt)("inlineCode",{parentName:"li"},"Int"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"String"),", or ",(0,r.kt)("inlineCode",{parentName:"li"},"Bool"),").\nFor ",(0,r.kt)("inlineCode",{parentName:"li"},"newtype"),"s we can only write one."),(0,r.kt)("li",{parentName:"ol"},"We can have alternative structures using ",(0,r.kt)("inlineCode",{parentName:"li"},"|"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"newtype"),"s have no\nalternatives.")),(0,r.kt)("p",null,"This is because ",(0,r.kt)("inlineCode",{parentName:"p"},"newtype")," is used to provide a type safe ",(0,r.kt)("strong",{parentName:"p"},"alias"),", and ",(0,r.kt)("inlineCode",{parentName:"p"},"data"),"\nis used to build a new ",(0,r.kt)("strong",{parentName:"p"},"composite")," type that can potentially have ",(0,r.kt)("em",{parentName:"p"},"alternatives"),"."),(0,r.kt)("p",null,"Let's see a few of examples of data types:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"Bool"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data Bool\n  = True\n  | False\n")),(0,r.kt)("p",{parentName:"li"},"We created a new data type named ",(0,r.kt)("inlineCode",{parentName:"p"},"Bool")," with the possible values ",(0,r.kt)("inlineCode",{parentName:"p"},"True")," or ",(0,r.kt)("inlineCode",{parentName:"p"},"False"),".\nIn this case we only have ",(0,r.kt)("em",{parentName:"p"},"constructor")," alternatives and none of the constructors\ncarry additional values. This is similar to enums in other languages.")),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"Person"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data Person\n  = Person String Int -- where the first is the name and the second is\n                      -- the age\n")),(0,r.kt)("p",{parentName:"li"},"We created a new data type named ",(0,r.kt)("inlineCode",{parentName:"p"},"Person"),". Values of the type ",(0,r.kt)("inlineCode",{parentName:"p"},"Person"),"\nlook like this:"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre"},"Person <some-string> <some-int>\n")),(0,r.kt)("p",{parentName:"li"},"For example:"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'Person "Gil" 32\n')),(0,r.kt)("p",{parentName:"li"},"In this case we create a ",(0,r.kt)("em",{parentName:"p"},"composite")," of multiple types, without alternatives.\nThis is similar to structs in other language, but structs give each field\na name, and here we distinguish them by position."),(0,r.kt)("p",{parentName:"li"},"Alternatively, Haskell has ",(0,r.kt)("em",{parentName:"p"},"syntactic sugar")," for naming fields called ",(0,r.kt)("strong",{parentName:"p"},"records"),".\nThe above definition can also be written like this:"))),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data Person\n  = Person\n    { name :: String\n    , age :: Int\n    }\n")),(0,r.kt)("p",null,"   Values of this type can be written exactly as before,"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'Person "Gil" 32\n')),(0,r.kt)("p",null,"   Or with this syntax:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'Person { name = "Gil", age = 32 }\n')),(0,r.kt)("p",null,"   Haskell will also generate functions that can be used to extract the fields from the composite type:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"name :: Person -> String\nage :: Person -> Int\n")),(0,r.kt)("p",null,"   Which can be used like this:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'ghci> age (Person { name = "Gil", age = 32 })\n32\n')),(0,r.kt)("p",null,"   We even have special syntax for updating specific fields in a record. Of course,\nwe do not update records in place - we generate a new value instead."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'ghci> gil = Person { name = "Gil", age = 32 }\nghci> age (gil { age = 33 })\n33\nghci> age gil\n32\n')),(0,r.kt)("p",null,"   Unfortunately, having specialized functions for each field also means that if we\ndefined a different data type with the field ",(0,r.kt)("inlineCode",{parentName:"p"},"age"),", the functions which GHC needs\nto generate will clash."),(0,r.kt)("p",null,"   The easiest way to solve this is to give fields unique names, for example\nby adding a prefix:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data Person\n  = Person\n    { pName :: String\n    , pAge :: Int\n    }\n")),(0,r.kt)("p",null,"   Another way is by using extensions to the Haskell language, which we will cover\nin later chapters."),(0,r.kt)("ol",{start:3},(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"Tuple"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data Tuple a b\n  = Tuple a b\n")),(0,r.kt)("p",{parentName:"li"},"This is pretty similar to ",(0,r.kt)("inlineCode",{parentName:"p"},"Person"),", but we can plug any type we want\nfor this definition. For example:"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"Tuple \"Clicked\" True :: Tuple String Bool\n\nTuple 'a' 'z' :: Tuple Char Char\n")),(0,r.kt)("p",{parentName:"li"},"This type has special syntax in Haskell:"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"(\"Clicked\", True) :: (String, Bool)\n\n('a', 'z') :: (Char, Char)\n")),(0,r.kt)("p",{parentName:"li"}," This ",(0,r.kt)("inlineCode",{parentName:"p"},"Tuple")," definition is polymorphic, we define the structure but are able to\nplug different types into the structure to get concrete types. You can think of ",(0,r.kt)("inlineCode",{parentName:"p"},"Tuple"),"\nas a ",(0,r.kt)("em",{parentName:"p"},"template")," for a data type waiting to be filled, or as a ",(0,r.kt)("strong",{parentName:"p"},"function"),' waiting\nfor types as input in order to return a data type. We can even take a look at the "type"\nsignature of ',(0,r.kt)("inlineCode",{parentName:"p"},"Tuple")," in ",(0,r.kt)("inlineCode",{parentName:"p"},"ghci")," using the ",(0,r.kt)("inlineCode",{parentName:"p"},":kind")," command."),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"ghci> data Tuple a b = Tuple a b\nghci> :kind Tuple\nTuple :: * -> * -> *\n")),(0,r.kt)("blockquote",{parentName:"li"},(0,r.kt)("h4",{parentName:"blockquote",id:"quick-detour-kinds"},"Quick detour: Kinds"),(0,r.kt)("p",{parentName:"blockquote"},"The ",(0,r.kt)("inlineCode",{parentName:"p"},":kind"),' command is called as such because the "type" of a type is called a ',(0,r.kt)("strong",{parentName:"p"},"kind"),".\nKinds can be one of two things, either a ",(0,r.kt)("inlineCode",{parentName:"p"},"*")," which means a saturated (or concrete) type,\nsuch as ",(0,r.kt)("inlineCode",{parentName:"p"},"Int")," or ",(0,r.kt)("inlineCode",{parentName:"p"},"Person"),", or an ",(0,r.kt)("inlineCode",{parentName:"p"},"->")," of two kinds, which is, as you might have guessed,\na type function, taking kind and returning a kind."),(0,r.kt)("p",{parentName:"blockquote"},"Note that only types that have the kind ",(0,r.kt)("inlineCode",{parentName:"p"},"*")," can have values. So for example while ",(0,r.kt)("inlineCode",{parentName:"p"},"Tuple Int"),"\nis a valid Haskell concept that has the ",(0,r.kt)("em",{parentName:"p"},"kind")," ",(0,r.kt)("inlineCode",{parentName:"p"},"* -> *"),', and we can write code that will\nwork "generically" for all types that have a certain kind (e.g. ',(0,r.kt)("inlineCode",{parentName:"p"},"* -> *"),"), we cannot\nconstruct a value that will have the kind ",(0,r.kt)("inlineCode",{parentName:"p"},"* -> *"),". All values have types, and all\ntypes that have values have the kind ",(0,r.kt)("inlineCode",{parentName:"p"},"*"),"."),(0,r.kt)("p",{parentName:"blockquote"},"We will talk more about kinds later, for now let's focus on types!"))),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"Either"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"data Either a b\n  = Left a\n  | Right b\n")),(0,r.kt)("p",{parentName:"li"},"Similar to Tuple but instead of having only one constructor, we have\ntwo. This means that we can choose which side we want. Here are a\ncouple of values of type ",(0,r.kt)("inlineCode",{parentName:"p"},"Either String Int"),":"),(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'Left "Hello"\n\nRight 17\n')),(0,r.kt)("p",{parentName:"li"},"This type is useful for modeling errors. Either we succeeded and got\nwhat we wanted (The ",(0,r.kt)("inlineCode",{parentName:"p"},"Right")," constructor with the value), or we didn't\nand got an error instead (The ",(0,r.kt)("inlineCode",{parentName:"p"},"Left")," constructor with a string or a\ncustom error type)."))),(0,r.kt)("p",null,"In our program we use ",(0,r.kt)("inlineCode",{parentName:"p"},"data")," types to model the different kinds of content types\nin our markup language. We tag each structure using the data constructor\nand provide the rest of the information (the paragraph text, the list items, etc.)\nin the ",(0,r.kt)("inlineCode",{parentName:"p"},"<types>")," section of the data declaration for each constructor:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"type Document\n  = [Structure]\n\ndata Structure\n  = Heading Natural String\n  | Paragraph String\n  | UnorderedList [String]\n  | OrderedList [String]\n  | CodeBlock [String]\n")),(0,r.kt)("p",null,"Note: ",(0,r.kt)("inlineCode",{parentName:"p"},"Natural")," is defined in the ",(0,r.kt)("inlineCode",{parentName:"p"},"base")," package but not exported from ",(0,r.kt)("inlineCode",{parentName:"p"},"Prelude"),".\nFind out which module to import ",(0,r.kt)("inlineCode",{parentName:"p"},"Natural")," by using ",(0,r.kt)("a",{parentName:"p",href:"https://hoogle.haskell.org"},"Hoogle"),"."),(0,r.kt)("hr",null),(0,r.kt)("h3",{id:"exercises"},"Exercises"),(0,r.kt)("p",null,"Represent the following markup documents as values of ",(0,r.kt)("inlineCode",{parentName:"p"},"Document"),":"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-org"},"Hello, world!\n"))),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-org"},"* Welcome\n\nTo this tutorial about Haskell.\n"))),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-org"},"Remember that multiple lines with no separation\nare grouped together to a single paragraph\nbut list items remain separate.\n\n# Item 1 of a list\n# Item 2 of the same list\n"))),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("pre",{parentName:"li"},(0,r.kt)("code",{parentName:"pre",className:"language-org"},'* Compiling programs with ghc\n\nRunning ghc invokes the Glasgow Haskell Compiler (GHC),\nand can be used to compile Haskell modules and programs into native\nexecutables and libraries.\n\nCreate a new Haskell source file named hello.hs, and write\nthe following code in it:\n\n> main = putStrLn "Hello, Haskell!"\n\nNow, we can compile the program by invoking ghc with the file name:\n\n> \u279c ghc hello.hs\n> [1 of 1] Compiling Main             ( hello.hs, hello.o )\n> Linking hello ...\n\nGHC created the following files:\n\n- hello.hi - Haskell interface file\n- hello.o - Object file, the output of the compiler before linking\n- hello (or hello.exe on Microsoft Windows) - A native runnable executable.\n\nGHC will produce an executable when the source file satisfies both conditions:\n\n# Defines the main function in the source file\n# Defines the module name to be Main, or does not have a module declaration\n\nOtherwise, it will only produce the .o and .hi files.\n')))),(0,r.kt)("p",null,"Solutions:"),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Solution 1"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'example1 :: Document\nexample1 =\n  [ Paragraph "Hello, world!"\n  ]\n'))),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Solution 2"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'example2 :: Document\nexample2 =\n  [ Heading 1 "Welcome"\n  , Paragraph "To this tutorial about Haskell."\n  ]\n'))),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Solution 3"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'example3 :: Document\nexample3 =\n  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."\n  , OrderedList\n    [ "Item 1 of a list"\n    , "Item 2 of the same list"\n    ]\n  ]\n'))),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Solution 4"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},'example4 :: Document\nexample4 =\n  [ Heading 1 "Compiling programs with ghc"\n  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."\n  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"\n  , CodeBlock\n    [ "main = putStrLn \\"Hello, Haskell!\\""\n    ]\n  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"\n  , CodeBlock\n    [ "\u279c ghc hello.hs"\n    , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"\n    , "Linking hello ..."\n    ]\n  , Paragraph "GHC created the following files:"\n  , UnorderedList\n    [ "hello.hi - Haskell interface file"\n    , "hello.o - Object file, the output of the compiler before linking"\n    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."\n    ]\n  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"\n  , OrderedList\n    [ "Defines the main function in the source file"\n    , "Defines the module name to be Main, or does not have a module declaration"\n    ]\n  , Paragraph "Otherwise, it will only produce the .o and .hi files."\n  ]\n'))),(0,r.kt)("p",null,"Add a new module named ",(0,r.kt)("inlineCode",{parentName:"p"},"Markup")," and add the data type definition to it.\nNote that in this case we ",(0,r.kt)("em",{parentName:"p"},"do")," want to export the constructors of ",(0,r.kt)("inlineCode",{parentName:"p"},"Structure"),"."),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Solution"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-haskell"},"-- Markup.hs\n\nmodule Markup\n  ( Document\n  , Structure(..)\n  )\nwhere\n\nimport Numeric.Natural\n\ntype Document\n  = [Structure]\n\ndata Structure\n  = Heading Natural String\n  | Paragraph String\n  | UnorderedList [String]\n  | OrderedList [String]\n  | CodeBlock [String]\n"))),(0,r.kt)("hr",null),(0,r.kt)("h2",{id:"translating-directly"},"Translating directly?"),(0,r.kt)("p",null,"You might ask \"Why do we even need to represent the markup as a type?\nWhy don't we convert it into HTML as soon as we parse it\ninstead?\". That's a good question and a valid strategy. The reason we\nfirst represent it as a Haskell type is for flexibility and modularity."),(0,r.kt)("p",null,"If the parsing code is coupled with HTML generation, we lose the\nability to pre-process the markup document. For example we might want\nto take only a small part of the document (for summary) and present\nit, or create a table of content from headings. Or maybe we'd like to\nadd other targets and not just HTML - maybe markdown format or a GUI reader?"),(0,r.kt)("p",null,"Parsing to an \"abstract data type\" (ADT) representation (one that does\nnot contain the details of the language, for example '#' for\nordered lists) gives us the freedom to do so much more than just\nconversion to HTML that it's usually worth it in my opinion unless you\nreally need to optimize the process."))}m.isMDXComponent=!0}}]);