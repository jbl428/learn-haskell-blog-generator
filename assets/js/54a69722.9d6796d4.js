"use strict";(self.webpackChunklearn_haskell_blog_generator=self.webpackChunklearn_haskell_blog_generator||[]).push([[5891],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>m});var r=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function a(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,i=function(e,t){if(null==e)return{};var n,r,i={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var s=r.createContext({}),p=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):a(a({},t),e)),n},d=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},u="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},h=r.forwardRef((function(e,t){var n=e.components,i=e.mdxType,o=e.originalType,s=e.parentName,d=l(e,["components","mdxType","originalType","parentName"]),u=p(n),h=i,m=u["".concat(s,".").concat(h)]||u[h]||c[h]||o;return n?r.createElement(m,a(a({ref:t},d),{},{components:n})):r.createElement(m,a({ref:t},d))}));function m(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var o=n.length,a=new Array(o);a[0]=h;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[u]="string"==typeof e?e:i,a[1]=l;for(var p=2;p<o;p++)a[p]=n[p];return r.createElement.apply(null,a)}return r.createElement.apply(null,n)}h.displayName="MDXCreateElement"},4866:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>a,default:()=>c,frontMatter:()=>o,metadata:()=>l,toc:()=>p});var r=n(7462),i=(n(7294),n(3905));const o={},a="Lets code already!",l={unversionedId:"errors_and_files/implementation",id:"errors_and_files/implementation",title:"Lets code already!",description:"This was a long info dump. Let's practice what we've learned. We want to:",source:"@site/docs/06-errors_and_files/04-implementation.md",sourceDirName:"06-errors_and_files",slug:"/errors_and_files/implementation",permalink:"/learn-haskell-blog-generator/docs/errors_and_files/implementation",draft:!1,editUrl:"https://github.com/jbl428/learn-haskell-blog-generator/tree/book/docs/06-errors_and_files/04-implementation.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Exceptions",permalink:"/learn-haskell-blog-generator/docs/errors_and_files/exceptions"},next:{title:"Summary",permalink:"/learn-haskell-blog-generator/docs/errors_and_files/summary"}},s={},p=[{value:"New module",id:"new-module",level:2},{value:"Converting a directory",id:"converting-a-directory",level:2},{value:"<code>getDirFilesAndContent</code>",id:"getdirfilesandcontent",level:3},{value:"<code>applyIoOnList</code>",id:"applyioonlist",level:4},{value:"<code>filterAndReportFailures</code>",id:"filterandreportfailures",level:4},{value:"<code>createOutputDirectoryOrExit</code>",id:"createoutputdirectoryorexit",level:3},{value:"<code>txtsToRenderedHtml</code>",id:"txtstorenderedhtml",level:3},{value:"<code>copyFiles</code> and <code>writeFiles</code>",id:"copyfiles-and-writefiles",level:3},{value:"Summary",id:"summary",level:2}],d={toc:p},u="wrapper";function c(e){let{components:t,...n}=e;return(0,i.kt)(u,(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"lets-code-already"},"Lets code already!"),(0,i.kt)("p",null,"This was a long info dump. Let's practice what we've learned. We want to:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Create the output directory"),(0,i.kt)("li",{parentName:"ul"},"Grab all file names in a directory"),(0,i.kt)("li",{parentName:"ul"},"Filter them according to their extension"),(0,i.kt)("li",{parentName:"ul"},"Process .txt files"),(0,i.kt)("li",{parentName:"ul"},"Copy other files without modification"),(0,i.kt)("li",{parentName:"ul"},"Parse each text file, build an index of the result,\nconvert the files to HTML, and write everything to the target directory")),(0,i.kt)("blockquote",null,(0,i.kt)("p",{parentName:"blockquote"},"Note: I did not write this code immediately in the final form it was presented.\nIt was an iterative process of writing code, refactoring, splitting functions, changing\ntype signatures, and more. When solving a coding problem, start small and simple,\ndo the thing that works, and refactor it when it makes sense and makes the code clearer\nand more modular. In Haskell we pride ourselves in our ability to refactor code and improve\nit over time, and that principle holds when writing new software as well!")),(0,i.kt)("h2",{id:"new-module"},"New module"),(0,i.kt)("p",null,"Let's create a new module, ",(0,i.kt)("inlineCode",{parentName:"p"},"HsBlog.Directory"),", which will be responsible for handling\ndirectories and multiple files. From this module we will export the ",(0,i.kt)("inlineCode",{parentName:"p"},"convertDirectory"),"\nand ",(0,i.kt)("inlineCode",{parentName:"p"},"buildIndex")," functions we've defined before:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Process multiple files and convert directories\n\nmodule HsBlog.Directory\n  ( convertDirectory\n  , buildIndex\n  )\n  where\n")),(0,i.kt)("p",null,"In this module we are going to use the\n",(0,i.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html"},"directory"),"\nand ",(0,i.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath.html"},"filepath"),"\nlibraries to manipulate directories, files and filepaths.\nWe'll use the new abstractions we've learned, ",(0,i.kt)("inlineCode",{parentName:"p"},"Traversable")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"Monad"),", and the concepts\nand types we've learned about: ",(0,i.kt)("inlineCode",{parentName:"p"},"Either"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"IO")," and exceptions."),(0,i.kt)("p",null,"For all of that, we need quite a few imports:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"import qualified HsBlog.Markup as Markup\nimport qualified HsBlog.Html as Html\nimport HsBlog.Convert (convert, convertStructure)\n\nimport Data.List (partition)\nimport Data.Traversable (for)\nimport Control.Monad (void, when)\n\nimport System.IO (hPutStrLn, stderr)\nimport Control.Exception (catch, displayException, SomeException(..))\nimport System.Exit (exitFailure)\nimport System.FilePath\n  ( takeExtension\n  , takeBaseName\n  , (<.>)\n  , (</>)\n  , takeFileName\n  )\nimport System.Directory\n  ( createDirectory\n  , removeDirectoryRecursive\n  , listDirectory\n  , doesDirectoryExist\n  , copyFile\n  )\n")),(0,i.kt)("p",null,"If you are unsure what a specific function we're using does, look it up at\n",(0,i.kt)("a",{parentName:"p",href:"https://hoogle.haskell.org/"},"Hoogle"),",\nread the type signature and the documentation, and play around with it in ",(0,i.kt)("inlineCode",{parentName:"p"},"ghci"),"."),(0,i.kt)("h2",{id:"converting-a-directory"},"Converting a directory"),(0,i.kt)("p",null,"We can start by describing the high-level function ",(0,i.kt)("inlineCode",{parentName:"p"},"convertDirectory")," which\nencapsulates many smaller functions, each responsible for doing a specific thing.\n",(0,i.kt)("inlineCode",{parentName:"p"},"convertDirectory")," is quite imperative looking, and looks like a different way to\ndescribe the steps of completing our task:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Copy files from one directory to another, converting '.txt' files to\n--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.\n--\n-- May throw an exception on output directory creation.\nconvertDirectory :: FilePath -> FilePath -> IO ()\nconvertDirectory inputDir outputDir = do\n  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir\n  createOutputDirectoryOrExit outputDir\n  let\n    outputHtmls = txtsToRenderedHtml filesToProcess\n  copyFiles outputDir filesToCopy\n  writeFiles outputDir outputHtmls\n  putStrLn \"Done.\"\n")),(0,i.kt)("p",null,"Here we trust that each ",(0,i.kt)("inlineCode",{parentName:"p"},"IO")," function handles errors responsibly,\nand terminates the project when necessary."),(0,i.kt)("p",null,"Let's examine the steps in order."),(0,i.kt)("h3",{id:"getdirfilesandcontent"},(0,i.kt)("inlineCode",{parentName:"h3"},"getDirFilesAndContent")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | The relevant directory content for our application\ndata DirContents\n  = DirContents\n    { dcFilesToProcess :: [(FilePath, String)]\n      -- ^ File paths and their content\n    , dcFilesToCopy :: [FilePath]\n      -- ^ Other file paths, to be copied directly\n    }\n\n-- | Returns the directory content\ngetDirFilesAndContent :: FilePath -> IO DirContents\n\n")),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"getDirFilesAndContent")," is responsible for providing the relevant files for processing --\nboth the ones we need to convert to markup (and their textual content) and other files we\nmight want to copy as-is (such as images and style-sheets):"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},'-- | Returns the directory content\ngetDirFilesAndContent :: FilePath -> IO DirContents\ngetDirFilesAndContent inputDir = do\n  files <- map (inputDir </>) <$> listDirectory inputDir\n  let\n    (txtFiles, otherFiles) =\n      partition ((== ".txt") . takeExtension) files\n  txtFilesAndContent <-\n    applyIoOnList readFile txtFiles >>= filterAndReportFailures\n  pure $ DirContents\n    { dcFilesToProcess = txtFilesAndContent\n    , dcFilesToCopy = otherFiles\n    }\n')),(0,i.kt)("p",null,"This function does 4 important things:"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"Lists all the files in the directory"),(0,i.kt)("li",{parentName:"ol"},"Splits the files into 2 groups according to their file extension"),(0,i.kt)("li",{parentName:"ol"},"Reads the contents of the .txt files and report when files fail to be read"),(0,i.kt)("li",{parentName:"ol"},"Returns the results. We've defined a data type to make the result content more obvious")),(0,i.kt)("p",null,"Part (3) is a little bit more involved than the rest, let's explore it."),(0,i.kt)("h4",{id:"applyioonlist"},(0,i.kt)("inlineCode",{parentName:"h4"},"applyIoOnList")),(0,i.kt)("hr",null),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"applyIoOnList")," has the following type signature:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]\n")),(0,i.kt)("p",null,"It tries to apply an ",(0,i.kt)("inlineCode",{parentName:"p"},"IO")," function on a list of values, and document successes and failures."),(0,i.kt)("p",null,"Try to implement it! If you need a hint for which functions to use, see the import list\nwe wrote earlier."),(0,i.kt)("details",null,(0,i.kt)("summary",null,"Answer"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Try to apply an IO function on a list of values, document successes and failures\napplyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]\napplyIoOnList action inputs = do\n  for inputs $ \\input -> do\n    maybeResult <-\n      catch\n        (Right <$> action input)\n        ( \\(SomeException e) -> do\n          pure $ Left (displayException e)\n        )\n    pure (input, maybeResult)\n"))),(0,i.kt)("hr",null),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"applyIoOnList")," is a higher order function that applies a particular ",(0,i.kt)("inlineCode",{parentName:"p"},"IO")," function\n(in our case ",(0,i.kt)("inlineCode",{parentName:"p"},"readFile"),") on a list of things (in our case ",(0,i.kt)("inlineCode",{parentName:"p"},"FilePath"),"s).\nFor each thing, it returns the thing itself along with the result of\napplying the ",(0,i.kt)("inlineCode",{parentName:"p"},"IO")," function as an ",(0,i.kt)("inlineCode",{parentName:"p"},"Either"),", where the ",(0,i.kt)("inlineCode",{parentName:"p"},"Left")," side is a ",(0,i.kt)("inlineCode",{parentName:"p"},"String"),"\nrepresentation of an error if one occurred."),(0,i.kt)("p",null,"Notice how much the type of this function tells us about what it might do.\nBecause the types are polymorphic, there is nothing else to do with\nthe ",(0,i.kt)("inlineCode",{parentName:"p"},"a"),"s other than apply them to the function, and nowhere to generate ",(0,i.kt)("inlineCode",{parentName:"p"},"b"),"\nfrom other than the result of the function."),(0,i.kt)("blockquote",null,(0,i.kt)("p",{parentName:"blockquote"},"Note: when I first wrote this function, it was specialized to work only on ",(0,i.kt)("inlineCode",{parentName:"p"},"readFile"),",\ntake specifically ",(0,i.kt)("inlineCode",{parentName:"p"},"[FilePath]")," and return ",(0,i.kt)("inlineCode",{parentName:"p"},"IO [(FilePath, Either String String)]"),".\nBut after running into other use cases where I could use it (",(0,i.kt)("inlineCode",{parentName:"p"},"writeFiles")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"copyFiles"),")\nI refactored out the ",(0,i.kt)("inlineCode",{parentName:"p"},"action"),", the input type and the return type.")),(0,i.kt)("p",null,"This function uses exceptions to catch any error that might be thrown, and encodes\nboth the failure and success cases in the type system using ",(0,i.kt)("inlineCode",{parentName:"p"},"Either"),", delaying\nthe handling of exceptions to the function caller while making sure it won't\nbe forgotten!"),(0,i.kt)("p",null,"Next, let's look at the function that handles the errors by reporting and then filtering out\nall the cases that failed."),(0,i.kt)("h4",{id:"filterandreportfailures"},(0,i.kt)("inlineCode",{parentName:"h4"},"filterAndReportFailures")),(0,i.kt)("hr",null),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"filterAndReportFailures")," has the following type signature:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]\n")),(0,i.kt)("p",null,"It filters out unsuccessful operations on files and reports errors to the stderr."),(0,i.kt)("p",null,"Try to implement it!"),(0,i.kt)("details",null,(0,i.kt)("summary",null,"Answer"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Filter out unsuccessful operations on files and report errors to stderr.\nfilterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]\nfilterAndReportFailures =\n  foldMap $ \\(file, contentOrErr) ->\n    case contentOrErr of\n      Left err -> do\n        hPutStrLn stderr err\n        pure []\n      Right content ->\n        pure [(file, content)]\n")),(0,i.kt)("p",null,"This code may seem a bit surprising - how come we can use ",(0,i.kt)("inlineCode",{parentName:"p"},"foldMap")," here? Reminder,\nthe type of ",(0,i.kt)("inlineCode",{parentName:"p"},"foldMap")," is:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m\n")),(0,i.kt)("p",null,"If we specialize this function for our use case, substituting the general type\nwith the types we are using, we learn that ",(0,i.kt)("inlineCode",{parentName:"p"},"IO [(a, b)]")," is a monoid.\nAnd indeed - ",(0,i.kt)("inlineCode",{parentName:"p"},"[a]")," is a monoid for any ",(0,i.kt)("inlineCode",{parentName:"p"},"a")," with ",(0,i.kt)("inlineCode",{parentName:"p"},"[]")," (the empty list) as ",(0,i.kt)("inlineCode",{parentName:"p"},"mempty"),"\nand ",(0,i.kt)("inlineCode",{parentName:"p"},"++")," as ",(0,i.kt)("inlineCode",{parentName:"p"},"<>"),", but also ",(0,i.kt)("inlineCode",{parentName:"p"},"IO a")," is a monoid for any ",(0,i.kt)("inlineCode",{parentName:"p"},"a")," that is itself\na monoid with ",(0,i.kt)("inlineCode",{parentName:"p"},"pure mempty")," as ",(0,i.kt)("inlineCode",{parentName:"p"},"mempty")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"liftA2 (<>)")," as ",(0,i.kt)("inlineCode",{parentName:"p"},"<>"),"!"),(0,i.kt)("p",null,"Using these instances, we can ",(0,i.kt)("inlineCode",{parentName:"p"},"map")," over the content, handle errors, and return\nan empty list to filter out a failed case, or a singleton list to keep the result.\nAnd the ",(0,i.kt)("inlineCode",{parentName:"p"},"fold")," in ",(0,i.kt)("inlineCode",{parentName:"p"},"foldMap")," will concatenate the resulting list where we return\nall of the successful cases!"),(0,i.kt)("p",null,"If you've written this in a different way that does the same thing, that's fine too!\nIt's just nice to see how sometimes abstractions can be used to write concise code.")),(0,i.kt)("hr",null),(0,i.kt)("p",null,"These functions are responsible for fetching the right information. Next,\nlet's look at the code for creating a new directory."),(0,i.kt)("h3",{id:"createoutputdirectoryorexit"},(0,i.kt)("inlineCode",{parentName:"h3"},"createOutputDirectoryOrExit")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},'-- | Creates an output directory or terminates the program\ncreateOutputDirectoryOrExit :: FilePath -> IO ()\ncreateOutputDirectoryOrExit outputDir =\n  whenIO\n    (not <$> createOutputDirectory outputDir)\n    (hPutStrLn stderr "Cancelled." *> exitFailure)\n\n-- | Creates the output directory.\n--   Returns whether the directory was created or not.\ncreateOutputDirectory :: FilePath -> IO Bool\ncreateOutputDirectory dir = do\n  dirExists <- doesDirectoryExist dir\n  create <-\n    if dirExists\n      then do\n        override <- confirm "Output directory exists. Override?"\n        when override (removeDirectoryRecursive dir)\n        pure override\n      else\n        pure True\n  when create (createDirectory dir)\n  pure create\n')),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"createOutputDirectoryOrExit")," itself is not terribly exciting, it does\nwhat it is named -- it tries to create the output directory, and exits the\nprogram in case it didn't succeed."),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"createOutputDirectory")," is the function that actually does the heavy lifting.\nIt checks if the directory already exists, and checks if the user would like to\noverride it. If they do, we remove it and create the new directory; if they don't,\nwe do nothing and report their decision."),(0,i.kt)("h3",{id:"txtstorenderedhtml"},(0,i.kt)("inlineCode",{parentName:"h3"},"txtsToRenderedHtml")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"let\n  outputHtmls = txtsToRenderedHtml filesToProcess\n")),(0,i.kt)("hr",null),(0,i.kt)("p",null,"In this part of the code we convert files to markup and change the\ninput file paths to their respective output file paths (",(0,i.kt)("inlineCode",{parentName:"p"},".txt")," -> ",(0,i.kt)("inlineCode",{parentName:"p"},".html"),").\nWe then build the index page, and convert everything to HTML."),(0,i.kt)("p",null,"Implement ",(0,i.kt)("inlineCode",{parentName:"p"},"txtsToRenderedHtml"),", which has the following type signature:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]\n")),(0,i.kt)("details",null,(0,i.kt)("summary",null,"Hint"),(0,i.kt)("p",null,"I implemented this by defining three functions:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]\n\ntoOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)\n\nconvertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)\n"))),(0,i.kt)("p",null,"."),(0,i.kt)("details",null,(0,i.kt)("summary",null,"Answer"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},'-- | Convert text files to Markup, build an index, and render as html.\ntxtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]\ntxtsToRenderedHtml txtFiles =\n  let\n    txtOutputFiles = map toOutputMarkupFile txtFiles\n    index = ("index.html", buildIndex txtOutputFiles)\n  in\n    map (fmap Html.render) (index : map convertFile txtOutputFiles)\n\ntoOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)\ntoOutputMarkupFile (file, content) =\n  (takeBaseName file <.> "html", Markup.parse content)\n\nconvertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)\nconvertFile (file, doc) = (file, convert file doc)\n')),(0,i.kt)("p",null,"One possibly surprising thing about this code could be the ",(0,i.kt)("inlineCode",{parentName:"p"},"map (fmap Html.render)"),"\npart. We can use ",(0,i.kt)("inlineCode",{parentName:"p"},"fmap")," on the tuple because it is a ",(0,i.kt)("inlineCode",{parentName:"p"},"Functor")," on the second\nargument, just like ",(0,i.kt)("inlineCode",{parentName:"p"},"Either"),"!")),(0,i.kt)("hr",null),(0,i.kt)("h3",{id:"copyfiles-and-writefiles"},(0,i.kt)("inlineCode",{parentName:"h3"},"copyFiles")," and ",(0,i.kt)("inlineCode",{parentName:"h3"},"writeFiles")),(0,i.kt)("p",null,"The only thing left to do is to write the directory\ncontent, after the processing is completed, to the newly created directory:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Copy files to a directory, recording errors to stderr.\ncopyFiles :: FilePath -> [FilePath] -> IO ()\ncopyFiles outputDir files = do\n  let\n    copyFromTo file = copyFile file (outputDir </> takeFileName file)\n  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures\n")),(0,i.kt)("p",null,"Here we use ",(0,i.kt)("inlineCode",{parentName:"p"},"applyIoOnList")," again to do something a bit more complicated,\ninstead of reading from a file, it copies from the input path to a newly generated\noutput path. Then we pass the result (which has the type ",(0,i.kt)("inlineCode",{parentName:"p"},"[(FilePath, Either String ())]"),")\nto ",(0,i.kt)("inlineCode",{parentName:"p"},"filterAndReportFailures")," to print the errors and filter out the unsuccessful copies.\nBecause we are not really interested in the output of ",(0,i.kt)("inlineCode",{parentName:"p"},"filterAndReportFailures"),",\nwe discard it with ",(0,i.kt)("inlineCode",{parentName:"p"},"void"),", returning ",(0,i.kt)("inlineCode",{parentName:"p"},"()")," as a result instead:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},"-- | Write files to a directory, recording errors to stderr.\nwriteFiles :: FilePath -> [(FilePath, String)] -> IO ()\nwriteFiles outputDir files = do\n  let\n    writeFileContent (file, content) =\n      writeFile (outputDir </> file) content\n  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures\n")),(0,i.kt)("p",null,"Once again, this code looks almost exactly like ",(0,i.kt)("inlineCode",{parentName:"p"},"copyFiles"),", but the types are different.\nHaskell's combination of parametric polymorphism + type class for abstractions is really\npowerful, and has helped us reduce quite a bit of code."),(0,i.kt)("hr",null),(0,i.kt)("p",null,"This pattern of using ",(0,i.kt)("inlineCode",{parentName:"p"},"applyIoOnList")," and then ",(0,i.kt)("inlineCode",{parentName:"p"},"filterAndReportFailures"),"\nhappens more than once. It might be a good candidate for refactoring. Try it!\nWhat do you think about the resulting code? Is it easier or more difficult to\nunderstand? Is it more modular or less? What are the pros and cons?"),(0,i.kt)("hr",null),(0,i.kt)("h2",{id:"summary"},"Summary"),(0,i.kt)("p",null,"With that, we have completed our ",(0,i.kt)("inlineCode",{parentName:"p"},"HsBlog.Directory")," module that is responsible for converting\na directory safely. Note that the code could probably be simplified quite a bit if we\nwere fine with errors crashing the entire program altogether, but sometimes this is\nthe price we pay for robustness. It is up to you to choose what you can live with\nand what not, but I hope this saga has taught you how to approach error handling\nin Haskell in case you need to."),(0,i.kt)("p",null,"View the full module:"),(0,i.kt)("details",null,(0,i.kt)("summary",null,"HsBlog.Directory"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-haskell"},'-- | Process multiple files and convert directories\n\nmodule HsBlog.Directory\n  ( convertDirectory\n  , buildIndex\n  )\n  where\n\nimport qualified HsBlog.Markup as Markup\nimport qualified HsBlog.Html as Html\nimport HsBlog.Convert (convert, convertStructure)\n\nimport Data.List (partition)\nimport Data.Traversable (for)\nimport Control.Monad (void, when)\n\nimport System.IO (hPutStrLn, stderr)\nimport Control.Exception (catch, displayException, SomeException(..))\nimport System.Exit (exitFailure)\nimport System.FilePath\n  ( takeExtension\n  , takeBaseName\n  , (<.>)\n  , (</>)\n  , takeFileName\n  )\nimport System.Directory\n  ( createDirectory\n  , removeDirectoryRecursive\n  , listDirectory\n  , doesDirectoryExist\n  , copyFile\n  )\n\n-- | Copy files from one directory to another, converting \'.txt\' files to\n--   \'.html\' files in the process. Recording unsuccessful reads and writes to stderr.\n--\n-- May throw an exception on output directory creation.\nconvertDirectory :: FilePath -> FilePath -> IO ()\nconvertDirectory inputDir outputDir = do\n  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir\n  createOutputDirectoryOrExit outputDir\n  let\n    outputHtmls = txtsToRenderedHtml filesToProcess\n  copyFiles outputDir filesToCopy\n  writeFiles outputDir outputHtmls\n  putStrLn "Done."\n\n------------------------------------\n-- * Read directory content\n\n-- | Returns the directory content\ngetDirFilesAndContent :: FilePath -> IO DirContents\ngetDirFilesAndContent inputDir = do\n  files <- map (inputDir </>) <$> listDirectory inputDir\n  let\n    (txtFiles, otherFiles) =\n      partition ((== ".txt") . takeExtension) files\n  txtFilesAndContent <-\n    applyIoOnList readFile txtFiles >>= filterAndReportFailures\n  pure $ DirContents\n    { dcFilesToProcess = txtFilesAndContent\n    , dcFilesToCopy = otherFiles\n    }\n\n-- | The relevant directory content for our application\ndata DirContents\n  = DirContents\n    { dcFilesToProcess :: [(FilePath, String)]\n      -- ^ File paths and their content\n    , dcFilesToCopy :: [FilePath]\n      -- ^ Other file paths, to be copied directly\n    }\n\n------------------------------------\n-- * Build index page\n\nbuildIndex :: [(FilePath, Markup.Document)] -> Html.Html\nbuildIndex files =\n  let\n    previews =\n      map\n        ( \\(file, doc) ->\n          case doc of\n            Markup.Heading 1 heading : article ->\n              Html.h_ 3 (Html.link_ file (Html.txt_ heading))\n                <> foldMap convertStructure (take 2 article)\n                <> Html.p_ (Html.link_ file (Html.txt_ "..."))\n            _ ->\n              Html.h_ 3 (Html.link_ file (Html.txt_ file))\n        )\n        files\n  in\n    Html.html_\n      "Blog"\n      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))\n        <> Html.h_ 2 (Html.txt_ "Posts")\n        <> mconcat previews\n      )\n\n------------------------------------\n-- * Conversion\n\n-- | Convert text files to Markup, build an index, and render as html.\ntxtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]\ntxtsToRenderedHtml txtFiles =\n  let\n    txtOutputFiles = map toOutputMarkupFile txtFiles\n    index = ("index.html", buildIndex txtOutputFiles)\n  in\n    map (fmap Html.render) (index : map convertFile txtOutputFiles)\n\ntoOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)\ntoOutputMarkupFile (file, content) =\n  (takeBaseName file <.> "html", Markup.parse content)\n\nconvertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)\nconvertFile (file, doc) = (file, convert file doc)\n\n------------------------------------\n-- * Output to directory\n\n-- | Creates an output directory or terminates the program\ncreateOutputDirectoryOrExit :: FilePath -> IO ()\ncreateOutputDirectoryOrExit outputDir =\n  whenIO\n    (not <$> createOutputDirectory outputDir)\n    (hPutStrLn stderr "Cancelled." *> exitFailure)\n\n-- | Creates the output directory.\n--   Returns whether the directory was created or not.\ncreateOutputDirectory :: FilePath -> IO Bool\ncreateOutputDirectory dir = do\n  dirExists <- doesDirectoryExist dir\n  create <-\n    if dirExists\n      then do\n        override <- confirm "Output directory exists. Override?"\n        when override (removeDirectoryRecursive dir)\n        pure override\n      else\n        pure True\n  when create (createDirectory dir)\n  pure create\n\n-- | Copy files to a directory, recording errors to stderr.\ncopyFiles :: FilePath -> [FilePath] -> IO ()\ncopyFiles outputDir files = do\n  let\n    copyFromTo file = copyFile file (outputDir </> takeFileName file)\n  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures\n\n-- | Write files to a directory, recording errors to stderr.\nwriteFiles :: FilePath -> [(FilePath, String)] -> IO ()\nwriteFiles outputDir files = do\n  let\n    writeFileContent (file, content) =\n      writeFile (outputDir </> file) content\n  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures\n\n------------------------------------\n-- * IO work and handling errors\n\n-- | Try to apply an IO function on a list of values, document successes and failures\napplyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]\napplyIoOnList action inputs = do\n  for inputs $ \\input -> do\n    maybeResult <-\n      catch\n        (Right <$> action input)\n        ( \\(SomeException e) -> do\n          pure $ Left (displayException e)\n        )\n    pure (input, maybeResult)\n\n-- | Filter out unsuccessful operations on files and report errors to stderr.\nfilterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]\nfilterAndReportFailures =\n  foldMap $ \\(file, contentOrErr) ->\n    case contentOrErr of\n      Left err -> do\n        hPutStrLn stderr err\n        pure []\n      Right content ->\n        pure [(file, content)]\n\n------------------------------------\n-- * Utilities\n\nconfirm :: String -> IO Bool\nconfirm question = do\n  putStrLn (question <> " (y/n)")\n  answer <- getLine\n  case answer of\n    "y" -> pure True\n    "n" -> pure False\n    _ -> do\n      putStrLn "Invalid response. Use y or n."\n      confirm question\n\nwhenIO :: IO Bool -> IO () -> IO ()\nwhenIO cond action = do\n  result <- cond\n  if result\n    then action\n    else pure ()\n'))))}c.isMDXComponent=!0}}]);