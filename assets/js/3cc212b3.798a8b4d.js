"use strict";(self.webpackChunklearn_haskell_blog_generator=self.webpackChunklearn_haskell_blog_generator||[]).push([[410],{3905:(e,t,r)=>{r.d(t,{Zo:()=>c,kt:()=>f});var n=r(7294);function l(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function o(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){l(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function i(e,t){if(null==e)return{};var r,n,l=function(e,t){if(null==e)return{};var r,n,l={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(l[r]=e[r]);return l}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(l[r]=e[r])}return l}var s=n.createContext({}),u=function(e){var t=n.useContext(s),r=t;return e&&(r="function"==typeof e?e(t):o(o({},t),e)),r},c=function(e){var t=u(e.components);return n.createElement(s.Provider,{value:t},e.children)},p="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var r=e.components,l=e.mdxType,a=e.originalType,s=e.parentName,c=i(e,["components","mdxType","originalType","parentName"]),p=u(r),m=l,f=p["".concat(s,".").concat(m)]||p[m]||d[m]||a;return r?n.createElement(f,o(o({ref:t},c),{},{components:r})):n.createElement(f,o({ref:t},c))}));function f(e,t){var r=arguments,l=t&&t.mdxType;if("string"==typeof e||l){var a=r.length,o=new Array(a);o[0]=m;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i[p]="string"==typeof e?e:l,o[1]=i;for(var u=2;u<a;u++)o[u]=r[u];return n.createElement.apply(null,o)}return n.createElement.apply(null,r)}m.displayName="MDXCreateElement"},6363:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>o,default:()=>d,frontMatter:()=>a,metadata:()=>i,toc:()=>u});var n=r(7462),l=(r(7294),r(3905));const a={},o="Exercises",i={unversionedId:"html/exercises",id:"html/exercises",title:"Exercises",description:"We need a few more features for our HTML library to be useful for",source:"@site/docs/03-html/08-exercises.md",sourceDirName:"03-html",slug:"/html/exercises",permalink:"/learn-haskell-blog-generator/html/exercises",draft:!1,editUrl:"https://github.com/jbl428/learn-haskell-blog-generator/tree/book/docs/03-html/08-exercises.md",tags:[],version:"current",sidebarPosition:8,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Exposing internal functionality (Internal modules)",permalink:"/learn-haskell-blog-generator/html/internal-modules"},next:{title:"Summary",permalink:"/learn-haskell-blog-generator/html/summary"}},s={},u=[{value:"1. Unordered lists",id:"1-unordered-lists",level:2},{value:"2. Ordered lists",id:"2-ordered-lists",level:2},{value:"3. Code blocks",id:"3-code-blocks",level:2},{value:"Solutions",id:"solutions",level:2}],c={toc:u},p="wrapper";function d(e){let{components:t,...r}=e;return(0,l.kt)(p,(0,n.Z)({},c,r,{components:t,mdxType:"MDXLayout"}),(0,l.kt)("h1",{id:"exercises"},"Exercises"),(0,l.kt)("p",null,"We need a few more features for our HTML library to be useful for\nour blog software. Add the following features to our ",(0,l.kt)("inlineCode",{parentName:"p"},"Html.Internal")," module\nand expose them from ",(0,l.kt)("inlineCode",{parentName:"p"},"Html"),"."),(0,l.kt)("h2",{id:"1-unordered-lists"},"1. Unordered lists"),(0,l.kt)("p",null,"These lists have the form:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-html"},"<ul>\n  <li>item 1</li>\n  <li>item 2</li>\n  <li>...</li>\n</ul>\n")),(0,l.kt)("p",null,"We want in our library a new function:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-haskell"},"ul_ :: [Structure] -> Structure\n")),(0,l.kt)("p",null,"So that users can write this:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-haskell"},'ul_\n  [ p_ "item 1"\n  , p_ "item 2"\n  , p_ "item 3"\n  ]\n')),(0,l.kt)("p",null,"and get this:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-html"},"<ul>\n  <li><p>item 1</p></li>\n  <li><p>item 2</p></li>\n  <li><p>item 3</p></li>\n</ul>\n")),(0,l.kt)("h2",{id:"2-ordered-lists"},"2. Ordered lists"),(0,l.kt)("p",null,"Very similar to unordered lists, but instead of ",(0,l.kt)("inlineCode",{parentName:"p"},"<ul>")," we use ",(0,l.kt)("inlineCode",{parentName:"p"},"<ol>")),(0,l.kt)("h2",{id:"3-code-blocks"},"3. Code blocks"),(0,l.kt)("p",null,"Very similar to ",(0,l.kt)("inlineCode",{parentName:"p"},"<p>"),", but use the ",(0,l.kt)("inlineCode",{parentName:"p"},"<pre>")," tag. Call this function ",(0,l.kt)("inlineCode",{parentName:"p"},"code_"),"."),(0,l.kt)("h2",{id:"solutions"},"Solutions"),(0,l.kt)("details",null,(0,l.kt)("summary",null,"Unordered lists"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-haskell"},'ul_ :: [Structure] -> Structure\nul_ =\n  Structure . el "ul" . concat . map (el "li" . getStructureString)\n'))),(0,l.kt)("details",null,(0,l.kt)("summary",null,"Ordered lists"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-haskell"},'ol_ :: [Structure] -> Structure\nol_ =\n  Structure . el "ol" . concat . map (el "li" . getStructureString)\n')),(0,l.kt)("p",null,"Note: the two functions above could be unified.")),(0,l.kt)("details",null,(0,l.kt)("summary",null,"Code blocks"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-haskell"},'code_ :: String -> Structure\ncode_ = Structure . el "pre" . escape\n'))))}d.isMDXComponent=!0}}]);