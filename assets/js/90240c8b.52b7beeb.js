"use strict";(self.webpackChunklearn_haskell_blog_generator=self.webpackChunklearn_haskell_blog_generator||[]).push([[203],{3905:(e,t,n)=>{n.d(t,{Zo:()=>h,kt:()=>g});var a=n(7294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var i=a.createContext({}),u=function(e){var t=a.useContext(i),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},h=function(e){var t=u(e.components);return a.createElement(i.Provider,{value:t},e.children)},p="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},c=a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,r=e.originalType,i=e.parentName,h=l(e,["components","mdxType","originalType","parentName"]),p=u(n),c=o,g=p["".concat(i,".").concat(c)]||p[c]||d[c]||r;return n?a.createElement(g,s(s({ref:t},h),{},{components:n})):a.createElement(g,s({ref:t},h))}));function g(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=n.length,s=new Array(r);s[0]=c;var l={};for(var i in t)hasOwnProperty.call(t,i)&&(l[i]=t[i]);l.originalType=e,l[p]="string"==typeof e?e:o,s[1]=l;for(var u=2;u<r;u++)s[u]=n[u];return a.createElement.apply(null,s)}return a.createElement.apply(null,n)}c.displayName="MDXCreateElement"},4031:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>i,contentTitle:()=>s,default:()=>d,frontMatter:()=>r,metadata:()=>l,toc:()=>u});var a=n(7462),o=(n(7294),n(3905));const r={},s="Frequently asked questions",l={unversionedId:"faq",id:"faq",title:"Frequently asked questions",description:"Got a question? You can ask in the book's issue tracker!",source:"@site/docs/12-faq.md",sourceDirName:".",slug:"/faq",permalink:"/learn-haskell-blog-generator/faq",draft:!1,editUrl:"https://github.com/jbl428/learn-haskell-blog-generator/tree/book/docs/12-faq.md",tags:[],version:"current",sidebarPosition:12,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Where to go next",permalink:"/learn-haskell-blog-generator/next"}},i={},u=[{value:"General questions",id:"general-questions",level:2},{value:"Why should I learn Haskell",id:"why-should-i-learn-haskell",level:3},{value:"How to install editor tools",id:"how-to-install-editor-tools",level:3},{value:"How to learn new things",id:"how-to-learn-new-things",level:3},{value:"Debugging",id:"debugging",level:2},{value:"How to debug Haskell code",id:"how-to-debug-haskell-code",level:3},{value:"How to understand type errors",id:"how-to-understand-type-errors",level:3},{value:"My program is slow. Why?",id:"my-program-is-slow-why",level:3},{value:"Design",id:"design",level:2},{value:"How to structure programs",id:"how-to-structure-programs",level:3},{value:"How to model data",id:"how-to-model-data",level:3}],h={toc:u},p="wrapper";function d(e){let{components:t,...n}=e;return(0,o.kt)(p,(0,a.Z)({},h,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"frequently-asked-questions"},"Frequently asked questions"),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},"Got a question? You can ask in the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/soupi/learn-haskell-blog-generator/issues"},"book's issue tracker"),"!")),(0,o.kt)("h2",{id:"general-questions"},"General questions"),(0,o.kt)("h3",{id:"why-should-i-learn-haskell"},"Why should I learn Haskell"),(0,o.kt)("p",null,"I've written a couple of articles on the topic:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://gilmi.me/blog/post/2020/04/28/consider-haskell"},"Consider Haskell")," (Alternative title, 'What can I do with Haskell?')"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://gilmi.me/blog/post/2022/12/13/learned-from-haskell"},"7 things I learned from Haskell"))),(0,o.kt)("h3",{id:"how-to-install-editor-tools"},"How to install editor tools"),(0,o.kt)("p",null,"As far as I know, the most recommended setup today for Haskell development is using\nVSCode or ",(0,o.kt)("a",{parentName:"p",href:"https://vscodium.com/"},"VSCodium")," together with the\nmarketplace ",(0,o.kt)("a",{parentName:"p",href:"https://marketplace.visualstudio.com/items?itemName=haskell.haskell"},"Haskell extension"),"."),(0,o.kt)("p",null,"The Haskell extension uses ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/haskell/haskell-language-server"},"haskell-language-server"),"\nwhich can be installed via ",(0,o.kt)("a",{parentName:"p",href:"https://www.haskell.org/ghcup/"},"GHCup")," or even via the Haskell extension itself."),(0,o.kt)("p",null,"If you already have a preferred editor,\n",(0,o.kt)("a",{parentName:"p",href:"https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor"},"see if HLS supports it"),",\nor alternatively use ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/ndmitchell/ghcid#readme"},"GHCid"),"\nwhich provides rapid feedback independently from an editor."),(0,o.kt)("h3",{id:"how-to-learn-new-things"},"How to learn new things"),(0,o.kt)("p",null,"The Haskell community keeps marching forward, developing new libraries, tools and techniques\nas well as creating new material for older concepts.\nThe ",(0,o.kt)("a",{parentName:"p",href:"https://haskell.pl-a.net"},"Haskell planetarium")," aggregates feeds from several communities into\none page, as well as a ",(0,o.kt)("a",{parentName:"p",href:"https://haskellweekly.news/"},"Haskell Weekly newsletter"),".\nYou might also find the quite a bit of Haskell presence on\n",(0,o.kt)("a",{parentName:"p",href:"https://twitter.com/search?q=%23Haskell&src=typeahead_click"},"Twitter"),"!"),(0,o.kt)("h2",{id:"debugging"},"Debugging"),(0,o.kt)("h3",{id:"how-to-debug-haskell-code"},"How to debug Haskell code"),(0,o.kt)("p",null,"Most imperative languages provide a step debugger. While the\n",(0,o.kt)("a",{parentName:"p",href:"https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#the-ghci-debugger"},"GHCi debugger"),",\nexists it is not particularly easy to use, especially because of Haskell's lazy evaluation where things\nmight not evaluated at the order we might intuitively expect. Because of that,\nHaskellers tend to use\n",(0,o.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/base-4.16.4.0/docs/Debug-Trace.html#g:1"},"trace debugging")," and\nequational reasoning. With trace debugging, we try to ",(0,o.kt)("em",{parentName:"p"},"verify our assumptions")," about the code -\nwe use the various ",(0,o.kt)("inlineCode",{parentName:"p"},"trace"),' functions as a "hack" to print variables, functions inputs, functions output\nor even just say "got here", from anywhere at the code.'),(0,o.kt)("p",null,'After finding something that does not match our assumptions, such as unexpected input or output\nof a function, we try to think what piece of code could be responsible for the discrepancy, or even use\ntrace debugging again to pinpoint the exact location, and try to use "equational reasoning" to\nevaluate the offending code that betrayed our expectations. If it\'s easy to do, we try running\nthe function in ',(0,o.kt)("inlineCode",{parentName:"p"},"ghci")," with different inputs to check our assumptions as well."),(0,o.kt)("p",null,'Because Haskell focuses on immutability, composibility and using types to eliminate many\nclasses of possible errors, "local reasoning" becomes possible, and trace debugging\nbecomes a viable strategy for debugging Haskell programs.'),(0,o.kt)("h3",{id:"how-to-understand-type-errors"},"How to understand type errors"),(0,o.kt)("p",null,"GHC type errors are often not the most friendly errors messages, but they mean well! They are just\ntrying to help us find inconsistencies in our code - often with regards to type usage, they help us\navoid making errors."),(0,o.kt)("p",null,"When you run into error messages, start by reading the messages themselves carefully\nuntil you get used to them, and then the offending code hinted by the error message.\nAs you gain experience, it is likely that the most important part of an error will be the location\nof the offending code, and by reading the code we can find the error without the actual error message."),(0,o.kt)("p",null,"Adding type signatures and annotations to test your understanding of the types also helps greatly.\nWe can even ask GHC for the expected type in a certain place by using\n",(0,o.kt)("a",{parentName:"p",href:"https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html"},"typed holes"),"."),(0,o.kt)("h3",{id:"my-program-is-slow-why"},"My program is slow. Why?"),(0,o.kt)("p",null,"There could be various reasons. From inefficient algorithms or\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/soupi/haskell-study-plan#data-structures"},"unsuited data structures")," for the task\nin terms of time complexity of the common operations, to less efficient memory representations\n(this is another reminder to use ",(0,o.kt)("inlineCode",{parentName:"p"},"Text")," over ",(0,o.kt)("inlineCode",{parentName:"p"},"String")," in most cases),\nand laziness issues (again, the evaluation strategy!)."),(0,o.kt)("p",null,"The ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/soupi/haskell-study-plan#performance"},"performance section")," in my Haskell\nstudy plan links to various resources on Haskell evaluation, profiling and case studies."),(0,o.kt)("h2",{id:"design"},"Design"),(0,o.kt)("h3",{id:"how-to-structure-programs"},"How to structure programs"),(0,o.kt)("p",null,"Start with the imperative shell functional core approach, define EDSLs with the combinator\npattern for logic if needed, use capabilities such as ",(0,o.kt)("inlineCode",{parentName:"p"},"State")," locally if needed,\nmaybe add an environment configuration with ",(0,o.kt)("inlineCode",{parentName:"p"},"ReaderT"),", see how it goes."),(0,o.kt)("p",null,"If that approach fails you, look at why it fails and examine other solutions according to your needs."),(0,o.kt)("h3",{id:"how-to-model-data"},"How to model data"),(0,o.kt)("p",null,"Modeling data using ADTs are usually the way to go. Often programmers coming from object oriented\nbackground tend to look at type classes as a way to define methods similar to inheritance,\nbut this often isn't the right approach and ADTs with different constructors for different alternatives\ngo a long way. Remember that even OOP people often preach for composition over inheritance."),(0,o.kt)("p",null,"Use functions to define behavior on data rather than trying to couple the two together."))}d.isMDXComponent=!0}}]);