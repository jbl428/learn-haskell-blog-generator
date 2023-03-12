"use strict";(self.webpackChunklearn_haskell_blog_generator=self.webpackChunklearn_haskell_blog_generator||[]).push([[5669],{3905:(e,n,t)=>{t.d(n,{Zo:()=>c,kt:()=>h});var a=t(7294);function o(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function r(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function l(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?r(Object(t),!0).forEach((function(n){o(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):r(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function i(e,n){if(null==e)return{};var t,a,o=function(e,n){if(null==e)return{};var t,a,o={},r=Object.keys(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||(o[t]=e[t]);return o}(e,n);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(o[t]=e[t])}return o}var s=a.createContext({}),d=function(e){var n=a.useContext(s),t=n;return e&&(t="function"==typeof e?e(n):l(l({},n),e)),t},c=function(e){var n=d(e.components);return a.createElement(s.Provider,{value:n},e.children)},u="mdxType",p={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},m=a.forwardRef((function(e,n){var t=e.components,o=e.mdxType,r=e.originalType,s=e.parentName,c=i(e,["components","mdxType","originalType","parentName"]),u=d(t),m=o,h=u["".concat(s,".").concat(m)]||u[m]||p[m]||r;return t?a.createElement(h,l(l({ref:n},c),{},{components:t})):a.createElement(h,l({ref:n},c))}));function h(e,n){var t=arguments,o=n&&n.mdxType;if("string"==typeof e||o){var r=t.length,l=new Array(r);l[0]=m;var i={};for(var s in n)hasOwnProperty.call(n,s)&&(i[s]=n[s]);i.originalType=e,i[u]="string"==typeof e?e:o,l[1]=i;for(var d=2;d<r;d++)l[d]=t[d];return a.createElement.apply(null,l)}return a.createElement.apply(null,t)}m.displayName="MDXCreateElement"},1252:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>s,contentTitle:()=>l,default:()=>p,frontMatter:()=>r,metadata:()=>i,toc:()=>d});var a=t(7462),o=(t(7294),t(3905));const r={},l="Generating documentation",i={unversionedId:"documentation",id:"documentation",title:"Generating documentation",description:"There are many ways",source:"@site/docs/09-documentation.md",sourceDirName:".",slug:"/documentation",permalink:"/learn-haskell-blog-generator/docs/documentation",draft:!1,editUrl:"https://github.com/jbl428/learn-haskell-blog-generator/tree/book/docs/09-documentation.md",tags:[],version:"current",sidebarPosition:9,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Testing",permalink:"/learn-haskell-blog-generator/docs/testing"},next:{title:"Recap",permalink:"/learn-haskell-blog-generator/docs/recap"}},s={},d=[{value:"Running Haddock",id:"running-haddock",level:2},{value:"Cabal",id:"cabal",level:3},{value:"Stack",id:"stack",level:3},{value:"Haddock coverage",id:"haddock-coverage",level:3},{value:"Haddock markup",id:"haddock-markup",level:2},{value:"Documenting definitions",id:"documenting-definitions",level:3},{value:"Section headings",id:"section-headings",level:3},{value:"Formatting",id:"formatting",level:3},{value:"More",id:"more",level:3},{value:"Summary",id:"summary",level:2}],c={toc:d},u="wrapper";function p(e){let{components:n,...t}=e;return(0,o.kt)(u,(0,a.Z)({},c,t,{components:n,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"generating-documentation"},"Generating documentation"),(0,o.kt)("p",null,"There are ",(0,o.kt)("a",{parentName:"p",href:"https://documentation.divio.com/"},"many ways"),"\nto help others to get started with our projects and libraries.\nFor example, we can write tutorials, provide runnable examples,\ndescribe the internals of the system, and create an API reference."),(0,o.kt)("p",null,"In this chapter we will focus on generating API reference pages (the kind that can be seen on Hackage)\nfrom annotated Haskell source code using ",(0,o.kt)("a",{parentName:"p",href:"https://www.haskell.org/haddock"},"Haddock"),"."),(0,o.kt)("h2",{id:"running-haddock"},"Running Haddock"),(0,o.kt)("p",null,"We can generate API reference pages (a.k.a. haddocks in the Haskell world) for our project\nusing our favorite package manager:"),(0,o.kt)("h3",{id:"cabal"},"Cabal"),(0,o.kt)("p",null,"We can run ",(0,o.kt)("inlineCode",{parentName:"p"},"cabal haddock")," to generate haddocks:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"\u279c cabal haddock\nResolving dependencies...\nBuild profile: -w ghc-9.0.1 -O1\nIn order, the following will be built (use -v for more details):\n - hs-blog-0.1.0.0 (lib) (first run)\nConfiguring library for hs-blog-0.1.0.0..\nPreprocessing library for hs-blog-0.1.0.0..\nRunning Haddock on library for hs-blog-0.1.0.0..\nHaddock coverage:\n   0% (  0 /  3) in 'HsBlog.Env'\n  Missing documentation for:\n    Module header\n    Env (src/HsBlog/Env.hs:3)\n    defaultEnv (src/HsBlog/Env.hs:10)\n  21% (  7 / 33) in 'HsBlog.Html.Internal'\n  Missing documentation for:\n    Module header\n    Html (src/HsBlog/Html/Internal.hs:8)\n...\nDocumentation created:\n/tmp/learn-haskell-blog-generator/dist-newstyle/build/x86_64-linux/ghc-9.0.1/hs-blog-0.1.0.0/doc/html/hs-blog/index.html\n")),(0,o.kt)("p",null,"Cabal and Haddock will build our project and generate HTML pages for us at:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-html"},"./dist-newstyle/build/<platform>/<compiler>/<package>-<version>/doc/html/<package>/\n")),(0,o.kt)("p",null,"We can then open the ",(0,o.kt)("inlineCode",{parentName:"p"},"index.html")," file from that directory in a web browser and view our package documentation."),(0,o.kt)("h3",{id:"stack"},"Stack"),(0,o.kt)("p",null,"We can run ",(0,o.kt)("inlineCode",{parentName:"p"},"stack haddock")," to generate haddocks:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"\u279c stack haddock\n...\nhs-blog> build (lib + exe)\nPreprocessing library for hs-blog-0.1.0.0..\nBuilding library for hs-blog-0.1.0.0..\n[1 of 7] Compiling HsBlog.Env\n[2 of 7] Compiling HsBlog.Html.Internal\n...\nhs-blog> haddock\nPreprocessing library for hs-blog-0.1.0.0..\nRunning Haddock on library for hs-blog-0.1.0.0..\nHaddock coverage:\n   0% (  0 /  3) in 'HsBlog.Env'\n  Missing documentation for:\n    Module header\n    Env (src/HsBlog/Env.hs:3)\n    defaultEnv (src/HsBlog/Env.hs:10)\n  21% (  7 / 33) in 'HsBlog.Html.Internal'\n  Missing documentation for:\n    Module header\n    Html (src/HsBlog/Html/Internal.hs:8)\n...\nDocumentation created:\n.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/doc/html/hs-blog/index.html,\n.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/doc/html/hs-blog/hs-blog.txt\nPreprocessing executable 'hs-blog-gen' for hs-blog-0.1.0.0..\n...\n")),(0,o.kt)("p",null,"Stack and Haddock will build our project and generate HTML pages for us at:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-html"},"./.stack-work/dist/<platform>/Cabal-<version>/doc/html/<package>/\n")),(0,o.kt)("p",null,"We can then open the ",(0,o.kt)("inlineCode",{parentName:"p"},"index.html")," file from that directory in a web browser and view our package documentation."),(0,o.kt)("h3",{id:"haddock-coverage"},"Haddock coverage"),(0,o.kt)("p",null,"Haddock will also output a coverage report when run, and will mention user-exposed constructs which are missing\ndocumentation. These constructs could be module headers, types, data constructors, type classes, functions, values, etc."),(0,o.kt)("p",null,"For example:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-haskell"},"Haddock coverage:\n...\n   0% (  0 /  3) in 'HsBlog.Convert'\n  Missing documentation for:\n    Module header\n    convert (src/HsBlog/Convert.hs:8)\n    convertStructure (src/HsBlog/Convert.hs:23)\n  67% (  2 /  3) in 'HsBlog.Directory'\n  Missing documentation for:\n    buildIndex (src/HsBlog/Directory.hs:80)\n...\n")),(0,o.kt)("p",null,"We can see that we did not document the ",(0,o.kt)("inlineCode",{parentName:"p"},"HsBlog.Convert")," at all, and we are missing\ndocumentation for the module header, the ",(0,o.kt)("inlineCode",{parentName:"p"},"convert")," function and the ",(0,o.kt)("inlineCode",{parentName:"p"},"convertStructure")," function."),(0,o.kt)("p",null,"On the other hand, it seems that we do currently have some documentation written for the ",(0,o.kt)("inlineCode",{parentName:"p"},"HsBlog.Directory"),"\nmodule! We'll see why, but first - try to generate haddocks, see the module hierarchy, browse around\nthe different modules, follow the links of the types, imagine what this API reference could look like,\nand let's see how we can improve it."),(0,o.kt)("h2",{id:"haddock-markup"},"Haddock markup"),(0,o.kt)("p",null,"Haddock builds the API reference pages by building our project, examining the exported modules\nand their exported definitions, and grabbing source code comments written in special markup format."),(0,o.kt)("p",null,"Let's take a quick look at this markup format. We will go over a few important bits,\nbut if you'd like to learn more, a complete guide for Haddock markup can be found in the\n",(0,o.kt)("a",{parentName:"p",href:"https://haskell-haddock.readthedocs.io/en/latest/markup.html"},"Haddock documentation"),"."),(0,o.kt)("h3",{id:"documenting-definitions"},"Documenting definitions"),(0,o.kt)("p",null,"All haddock annotations appear as part of regular Haskell comments.\nThey can be used with both single line form (",(0,o.kt)("inlineCode",{parentName:"p"},"--"),") and multi-line form (",(0,o.kt)("inlineCode",{parentName:"p"},"{-")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"-}"),").\nThe placement of a comment block and the haddock marker determine to which Haskell\ndefinition the haddock string is attached."),(0,o.kt)("p",null,"We can annotate a Haskell definition by writing a comment block prefixed with ",(0,o.kt)("inlineCode",{parentName:"p"},"|")," ",(0,o.kt)("em",{parentName:"p"},"before"),"\nthe definition, or by writing a comment block prefixed with ",(0,o.kt)("inlineCode",{parentName:"p"},"^")," ",(0,o.kt)("em",{parentName:"p"},"after")," the definition."),(0,o.kt)("p",null,"For example:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Construct an HTML page from a `Head`\n--   and a `Structure`.\nhtml_\n  :: Head -- ^ Represents the @\\<head\\>@ section in an HTML file\n  -> Structure -- ^ Represents the @\\<body\\>@ section in an HTML file\n  -> Html\nhtml_ = ...\n...\n")),(0,o.kt)("p",null,"Here's another example:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-haskell"},"{- | Represents a single markup structure. Such as:\n\n- A paragraph\n- An unordered list\n- A code block\n-}\ndata Structure\n  = Heading Natural String\n  -- ^ A section heading with a level\n  | Paragraph String\n  -- ^ A paragraph\n  | UnorderedList [String]\n  -- ^ An unordered list of strings\n  | OrderedList [String]\n  -- ^ An ordered list of strings\n  | CodeBlock [String]\n  -- ^ A code block\n")),(0,o.kt)("p",null,"And another:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-haskell"},"{- | Markup to HTML conversion module.\n\nThis module handles converting documents written in our custom\nMarkup language into HTML pages.\n-}\nmodule HsBlog.Convert where\n")),(0,o.kt)("p",null,"As you can see, ",(0,o.kt)("inlineCode",{parentName:"p"},"|")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"^")," can be used to document functions, function arguments,\ntypes, data constructors, modules, and more. They are probably the most important\nHaddock annotations to remember (and even then, ",(0,o.kt)("inlineCode",{parentName:"p"},"|")," alone will suffice)."),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},(0,o.kt)("strong",{parentName:"p"},"Tip"),": Annotate the modules, types, and the top-level definitions\nwhich are exported from your project\nwith some high-level description of what they are used for (at the very least)."),(0,o.kt)("p",{parentName:"blockquote"},"Your users and collaborators will thank you!")),(0,o.kt)("h3",{id:"section-headings"},"Section headings"),(0,o.kt)("p",null,"We can separate our module into sections by adding headings.\nHeadings are comments which are prefixed with a number of ",(0,o.kt)("inlineCode",{parentName:"p"},"*")," (just like in our markup language)."),(0,o.kt)("p",null,"For example:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-haskell"},"-- * HTML EDSL\n\nhtml_ :: Head -> Structure -> Html\nhtml_ = ...\n\n-- ** Structure\n\np_ :: Content -> Structure\np_ = ..\n\nh_ :: Content -> Structure\nh_ = ..\n\n...\n\n-- ** Content\n\ntxt_ :: String -> Content\ntxt_ = ...\n\nlink_ :: FilePath -> Content -> Content\nlink_ = ...\n")),(0,o.kt)("p",null,"It is also possible to add headings to the export list instead:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-haskell"},"module HsBlog.Html\n  ( -- * HTML EDSL\n    Html\n  , html_\n\n    -- ** Combinators used to construct the @\\<head\\>@ section\n  , Head\n  , title_\n  , stylesheet_\n  , meta_\n\n    -- ** Combinators used to construct the @\\<body\\>@ section\n  , Structure\n  , p_\n  , h_\n  , ul_\n  , ol_\n  , code_\n\n    -- ** Combinators used to construct content inside structures\n  , Content\n  , txt_\n  , img_\n  , link_\n  , b_\n  , i_\n\n    -- ** Render HTML to String\n  , render\n  )\n  where\n")),(0,o.kt)("p",null,"Separating parts of the module into sections helps keeping the important things together\nand Haddock will create a table-of-contents at the top of a module page for us as well."),(0,o.kt)("p",null,"Sometimes it's also easier to figure out whether a module should be split into multiple\nmodules or not after splitting it into sections using headings."),(0,o.kt)("hr",null),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"Exercise"),": Try to re-arrange the modules in our project to your liking and add headings to sections."),(0,o.kt)("hr",null),(0,o.kt)("h3",{id:"formatting"},"Formatting"),(0,o.kt)("p",null,"As we saw earlier, we can also add formatting in the content of our comments.\nFor example, we can:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Hyperlink identifiers by surrounding them with ",(0,o.kt)("inlineCode",{parentName:"p"},"`")),(0,o.kt)("p",{parentName:"li"},"For example: ",(0,o.kt)("inlineCode",{parentName:"p"},"`Heading`"))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Write ",(0,o.kt)("inlineCode",{parentName:"p"},"monospaced text")," by surrounding it with ",(0,o.kt)("inlineCode",{parentName:"p"},"@")),(0,o.kt)("p",{parentName:"li"},"For example: ",(0,o.kt)("inlineCode",{parentName:"p"},'@Paragraph "Hello"@'))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Add ",(0,o.kt)("em",{parentName:"p"},"emphasis")," to text by surrounding it with ",(0,o.kt)("inlineCode",{parentName:"p"},"/")),(0,o.kt)("p",{parentName:"li"},"For example: ",(0,o.kt)("inlineCode",{parentName:"p"},"/this is emphasised/"))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Add ",(0,o.kt)("strong",{parentName:"p"},"bold")," to text by surrounding it with ",(0,o.kt)("inlineCode",{parentName:"p"},"__")),(0,o.kt)("p",{parentName:"li"},"For example: ",(0,o.kt)("inlineCode",{parentName:"p"},"__this is bold__")))),(0,o.kt)("h3",{id:"more"},"More"),(0,o.kt)("p",null,"In this chapter we've covered the basics of the Haddock markup language.\nIf you'd like to know more, the ",(0,o.kt)("a",{parentName:"p",href:"https://haskell-haddock.readthedocs.io/en/latest/markup.html"},"Haddock markup guide"),"\ncontains information on how to create even more interesting documentation structures, such as\ncode blocks, grid tables, images and examples."),(0,o.kt)("h2",{id:"summary"},"Summary"),(0,o.kt)("p",null,"We've briefly covered one aspect of documenting Haskell programs:\nusing Haddock to generate informative API reference pages created from source code\ncomments which are annotated with Haddock markup."),(0,o.kt)("p",null,"While API references are incredibly valuable, remember that there are other forms of\ndocumentation that can help your users get started quickly, such as examples and tutorials."),(0,o.kt)("hr",null),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"Exercise"),": Add haddock annotation to the top-level definitions in our project and test your understanding\nof the program and the various parts - sometimes the best way to learn something is to try explaining it!"),(0,o.kt)("hr",null))}p.isMDXComponent=!0}}]);