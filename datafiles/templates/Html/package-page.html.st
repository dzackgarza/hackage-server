<!DOCTYPE html>
<html>
  <head>
    $hackageCssTheme()$
    <title>$docTitle$ | Hackage</title>
  </head>

  <body>
    <div id="page-header">
      <ul class="links" id="page-menu">
        <li>
          <a href="/">Home</a>
        </li>
        <li>
          <a href="/packages/">Browse</a>
        </li>
        <li>
          <a href="/accounts">User accounts</a>
        </li>
        <li>
          <form action="/packages/search" class="search" method="get">
            <button type="submit">Search</button>
            <input type="text" name="terms">
          </form>
        </li>
      </ul>
      <p class="caption"> Hackage :: Agda</p>
    </div>
    <div id="content" class="package">
      <div id="sidebar">
        <div id="install">
          <h3>Install</h3>
          <p id="license">License: <a href>BSD3</a></p>
          <p id="install-cabal">> cabal install Agda</p><br>
          <p id="install-distro"><span>Distributions</span><br>
            Debian (<a href>2.2.6</a>) | Arch (
            <a href>2.2.6</a>)</p>
          <p id="install-other"><a href="http://code.haskell.org/~duncan/hackage-server-new-chrome/package-2.html#">Source Tarball</a> | <a href="http://code.haskell.org/~duncan/hackage-server-new-chrome/package-2.html#">Package Description</a> | <a href="http://code.haskell.org/~duncan/hackage-server-new-chrome/package-2.html#">Haddocks</a></p>
        </div>
        <ul id="information">
          <li>
            <h3>Homepage</h3>
            <p><a href="http://code.haskell.org/~duncan/hackage-server-new-chrome/package-2.html#">http://wiki.portal.chalmers.se/agda/</a></p>
          </li>
          <li>
            <h3>Bug Reports</h3>
            <p><a href="http://code.haskell.org/~duncan/hackage-server-new-chrome/package-2.html#">http://code.google.com/p/agda/issues/list</a></p>
          </li>
          <li>
            <h3>Source</h3>
            <p><a href>https://github.com/agda/agda</a></p>
          </li>
          <li>
            <hr>
          </li>
          <li>
            <h3>Categories</h3>
            <p><a href="/packages/#cat:Data">Data</a>, <a href="/packages/#cat:Lenses">Lenses</a>, <a href="/packages/#cat:Generics">Generics</a></p>
          </li>
        </ul>
        <div id="properties">
          <h3>Properties</h3>
          <table class="properties">
            <tbody>
              <tr>
                <th>Changelog</th>
                <td>
                  <a href="/package/Agda-2.4.2.3/changelog">CHANGELOG</a>
                </td>
              </tr>
              <tr>
                <th>Copyright</th>
                <td>Copyright (C) 2012-2015 Edward A. Kmett</td>
              </tr>
              <tr>
                <th>Author</th>
                <td>Edward A. Kmett</td>
              </tr>
              <tr>
                <th>Maintainer</th>
                <td>Edward A. Kmett <<a href>ekmett@gmail.com</a>></td>
              </tr>
              <tr>
                <th>Stability</th>
                <td>Provisional</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
      <div id="content-main">
        <div>
          <h2>Agda</h2>
          <ul id="versions">
            <li id="current-version">2.2.6</li>
            <li>2.2.4</li>
            <li>2.2.2</li>
            <li>2.2.0</li>
            <li>...</li>
          </ul>
          <span class="view-all-versions"><i><a href>View All Versions</a></i><a href> >></a></span><span style="float:left; font-size: 10px">Current Version: 2.2.8</span><br>
          <span style="float:left; font-size: 10px">Last Upload: 2015-05-29 13:23:15 UTC</span><br></div>
        <div class="content">
          <p id="synopsis">A dependently typed functional programming language and proof assistant.<br>
            <a href="#">[Skip to ReadMe]</a></p>
          <div id="tags" style="margin-bottom: 20px">
            <h3>Tags:</h3>
            <ul>
              <li class="system-tag">library</li>
              <li class="system-tag">program</li>
              <li class="system-tag">dependent-types</li>
            </ul>
          </div>
          <input type="checkbox" id="header1" class="toggle-box checkmark">
          <label for="header1" class="toggle-box">
            <span></span>
          </label>
          <div id="description" class="desc">
            <p></p>
            <p>This package comes "Batteries Included" with many useful lenses for the types commonly used from the Haskell Platform, and with tools for automatically generating lenses and isomorphisms for user-supplied data types.</p>
            <p>The combinators in <tt>Control.Lens</tt> provide a highly generic toolbox for composing families of getters, folds, isomorphisms, traversals, setters and lenses and their indexed variants.</p>
            <p>An overview, with a large number of examples can be found in the <a href="https://github.com/ekmett/lens#lens-lenses-folds-and-traversals">README</a>.</p>
            <p>An introductory video on the style of code used in this library by Simon Peyton Jones is available from <a href="http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation">Skills Matter</a>.</p>
            <p>A video on how to use lenses and how they are constructed is available on <a href="http://youtu.be/cefnmjtAolY?hd=1">youtube</a>.</p>
            <p>Slides for that second talk can be obtained from <a href="http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf">comonad.com</a>.</p>
            <p>More information on the care and feeding of lenses, including a brief tutorial and motivation for their types can be found on the <a href="https://github.com/ekmett/lens/wiki">lens wiki</a>.</p>
            <p>A small game of <tt>pong</tt> and other more complex examples that manage their state using lenses can be found in the <a href="https://github.com/ekmett/lens/blob/master/examples/">example folder</a>.</p>
            <p><em>Lenses, Folds and Traversals</em></p>
            <p>With some signatures simplified, the core of the hierarchy of lens-like constructions looks like:</p>
            <p><img src="http://i.imgur.com/ALlbPRa.png" width="100%"></p>
            <p><a href="Hierarchy.png">(Local Copy)</a></p>
            <p>You can compose any two elements of the hierarchy above using <tt>(.)</tt> from the <tt>Prelude</tt>, and you can use any element of the hierarchy as any type it linked to above it.</p>
            <p>The result is their lowest upper bound in the hierarchy (or an error if that bound doesn't exist).</p>
            <p>For instance:</p>
            <ul>
              <li>
                <p>You can use any <tt>Traversal</tt> as a <tt>Fold</tt> or as a <tt>Setter</tt>.</p>
              </li>
              <li>
                <p>The composition of a <tt>Traversal</tt> and a <tt>Getter</tt> yields a <tt>Fold</tt>.</p>
              </li>
            </ul>
            <p><em>Minimizing Dependencies</em></p>
            <p>If you want to provide lenses and traversals for your own types in your own libraries, then you can do so without incurring a dependency on this (or any other) lens package at all.</p>
            <p><em>e.g.</em> for a data type:</p>
              <pre> data Foo a = Foo Int Int a</pre>
            <p>You can define lenses such as</p>
            <p>without the need to use any type that isn't already defined in the <tt>Prelude</tt>.</p>
            <p>And you can define a traversal of multiple fields with <tt>Control.Applicative.Applicative</tt>:</p>
            <p>What is provided in this library is a number of stock lenses and traversals for common haskell types, a wide array of combinators for working them, and more exotic functionality, (<em>e.g.</em> getters, setters, indexed folds, isomorphisms).</p>
            <div class="read-more"></div>
          </div>
          <hr>
          <div id="modules">
            <div style="float:right">
              <div class="right-mod" style="float:left">
                <h3>Dependencies</h3>
                <ul>
                  <li>
                    <a href="/package/array">array</a> (>=0.3.0.2 && <0.6),</li>
                  <li>
                    <a href="/package/base">base</a> (>=4.5 && <5),</li>
                  <li>
                    <a href="/package/base-orphans">base-orphans</a> (>=0.3 && <1),</li>
                  <li>
                    <a href="/package/bifunctors">bifunctors</a> (==5.*),</li>
                  <li>
                    <a href="/package/bytestring">bytestring</a> (>=0.9.1.10 && <0.11),</li>
                  <li>
                    <a href="/package/comonad">comonad</a> (==4.*),</li>
                  <li>
                    <a href="/package/containers">containers</a> (>=0.4.0 && <0.6),</li>
                  <li>
                    <a href="/package/contravariant">contravariant</a> (>=0.3 && <2),</li>
                  <li>
                    <a href="/package/distributive">distributive</a> (>=0.3 && <1),</li>
                  <li>
                    <a href="/package/exceptions">exceptions</a> (>=0.1.1 && <1),</li>
                  <li>
                    <a href="/package/filepath">filepath</a> (>=1.2.0.0 && <1.5),</li>
                  <li>
                    <a href="/package/free">free</a> (==4.*),</li>
                  <li>
                    <a href="/package/ghc-prim">ghc-prim</a>, <a href="/package/hashable">hashable</a> (>=1.1.2.3 && <1.3),</li>
                  <li>
                    <a href="/package/kan-extensions">kan-extensions</a> (>=4.2.1 && <5),</li>
                  <li>
                    <a href="/package/mtl">mtl</a> (>=2.0.1 && <2.3),</li>
                  <li>
                    <a href="/package/parallel">parallel</a> (>=3.1.0.1 && <3.3),</li>
                  <li>
                    <a href="/package/profunctors">profunctors</a> (==5.*),</li>
                  <li>
                    <a href="/package/reflection">reflection</a> (==2.*),</li>
                  <li>
                    <a href="/package/semigroupoids">semigroupoids</a> (==5.*),</li>
                  <li>
                    <a href="/package/semigroups">semigroups</a> (>=0.8.4 && <1),</li>
                  <li>
                    <a href="/package/tagged">tagged</a> (>=0.4.4 && <1),</li>
                  <li>
                    <a href="/package/template-haskell">template-haskell</a> (>=2.4 && <2.11),</li>
                  <li>
                    <a href="/package/text">text</a> (>=0.11 && <1.3),</li>
                  <li>
                    <a href="/package/transformers">transformers</a> (>=0.2 && <0.5),</li>
                  <li>
                    <a href="/package/transformers-compat">transformers-compat</a> (>=0.4 && <1),</li>
                  <li>
                    <a href="/package/unordered-containers">unordered-containers</a> (>=0.2.4 && <0.3),</li>
                  <li>
                    <a href="/package/vector">vector</a> (>=0.9 && <0.12),</li>
                  <li>
                    <a href="/package/void">void</a> (>=0.5 && <1)</li>
                </ul>
              </div>
            </div>
            <div>
              <h3 style="margin: 2em 0em 0em 0em;">Modules</h3>
              <ul class="modules">
                <li>
                  <i>Agda</i>
                  <ul class="modules">
                    <li>
                      <i>Auto</i>
                      <ul class="modules">
                        <li>Agda.Auto.Auto</li>
                        <li>Agda.Auto.Convert</li>
                        <li>Agda.Auto.NarrowingSearch</li>
                        <li>Agda.Auto.Print</li>
                        <li>Agda.Auto.SearchControl</li>
                        <li>Agda.Auto.Syntax</li>
                        <li>Agda.Auto.Typecheck</li>
                      </ul>
                    </li>
                    <li>
                      <i>Compiler</i>
                      <ul class="modules">
                        <li>
                          <i>Agate</i>
                          <ul class="modules">
                            <li>Agda.Compiler.Agate.Classify</li>
                            <li>Agda.Compiler.Agate.Common</li>
                            <li>Agda.Compiler.Agate.Main</li>
                            <li>Agda.Compiler.Agate.OptimizedPrinter</li>
                            <li>Agda.Compiler.Agate.TranslateName</li>
                            <li>Agda.Compiler.Agate.UntypedPrinter</li>
                          </ul>
                        </li>
                        <li>
                          <i>Alonzo</i>
                          <ul class="modules">
                            <li>Agda.Compiler.Alonzo.Haskell</li>
                            <li>Agda.Compiler.Alonzo.Main</li>
                            <li>Agda.Compiler.Alonzo.Names</li>
                            <li>Agda.Compiler.Alonzo.PatternMonad</li>
                          </ul>
                        </li>
                        <li>Agda.Compiler.HaskellTypes</li>
                        <li>
                          <i>MAlonzo</i>
                          <ul class="modules">
                            <li>Agda.Compiler.MAlonzo.Compiler</li>
                            <li>Agda.Compiler.MAlonzo.Encode</li>
                            <li>Agda.Compiler.MAlonzo.Misc</li>
                            <li>Agda.Compiler.MAlonzo.Pretty</li>
                            <li>Agda.Compiler.MAlonzo.Primitives</li>
                          </ul>
                        </li>
                      </ul>
                    </li>
                    <li>
                      <i>Interaction</i>
                      <ul class="modules">
                        <li>Agda.Interaction.BasicOps</li>
                        <li>
                          <i>CommandLine</i>
                          <ul class="modules">
                            <li>Agda.Interaction.CommandLine.CommandLine</li>
                          </ul>
                        </li>
                        <li>Agda.Interaction.Exceptions</li>
                        <li>Agda.Interaction.FindFile</li>
                        <li>Agda.Interaction.GhciTop</li>
                        <li>
                          <i>Highlighting</i>
                          <ul class="modules">
                            <li>Agda.Interaction.Highlighting.Emacs</li>
                            <li>Agda.Interaction.Highlighting.Generate</li>
                            <li>Agda.Interaction.Highlighting.HTML</li>
                            <li>Agda.Interaction.Highlighting.Precise</li>
                            <li>Agda.Interaction.Highlighting.Range</li>
                            <li>Agda.Interaction.Highlighting.Vim</li>
                          </ul>
                        </li>
                        <li>Agda.Interaction.Imports</li>
                        <li>Agda.Interaction.MakeCase</li>
                        <li>Agda.Interaction.Monad</li>
                        <li>Agda.Interaction.Options</li>
                      </ul>
                    </li>
                    <li>Agda.Main</li>
                    <li>
                      <i>Syntax</i>
                      <ul class="modules">
                        <li>Agda.Syntax.Abstract
                          <ul class="modules">
                            <li>Agda.Syntax.Abstract.Name</li>
                            <li>Agda.Syntax.Abstract.Pretty</li>
                            <li>Agda.Syntax.Abstract.Views</li>
                          </ul>
                        </li>
                        <li>Agda.Syntax.Common</li>
                        <li>Agda.Syntax.Concrete
                          <ul class="modules">
                            <li>Agda.Syntax.Concrete.Definitions</li>
                            <li>Agda.Syntax.Concrete.Name</li>
                            <li>Agda.Syntax.Concrete.Operators
                              <ul class="modules">
                                <li>Agda.Syntax.Concrete.Operators.Parser</li>
                              </ul>
                            </li>
                            <li>Agda.Syntax.Concrete.Pretty</li>
                          </ul>
                        </li>
                        <li>Agda.Syntax.Fixity</li>
                        <li>Agda.Syntax.Info</li>
                        <li>Agda.Syntax.Internal
                          <ul class="modules">
                            <li>Agda.Syntax.Internal.Generic</li>
                            <li>Agda.Syntax.Internal.Pattern</li>
                          </ul>
                        </li>
                        <li>Agda.Syntax.Literal</li>
                        <li>Agda.Syntax.Parser
                          <ul class="modules">
                            <li>Agda.Syntax.Parser.Alex</li>
                            <li>Agda.Syntax.Parser.Comments</li>
                            <li>Agda.Syntax.Parser.Layout</li>
                            <li>Agda.Syntax.Parser.LexActions</li>
                            <li>Agda.Syntax.Parser.Lexer</li>
                            <li>Agda.Syntax.Parser.LookAhead</li>
                            <li>Agda.Syntax.Parser.Monad</li>
                            <li>Agda.Syntax.Parser.Parser</li>
                            <li>Agda.Syntax.Parser.StringLiterals</li>
                            <li>Agda.Syntax.Parser.Tokens</li>
                          </ul>
                        </li>
                        <li>Agda.Syntax.Position</li>
                        <li>
                          <i>Scope</i>
                          <ul class="modules">
                            <li>Agda.Syntax.Scope.Base</li>
                            <li>Agda.Syntax.Scope.Monad</li>
                          </ul>
                        </li>
                        <li>Agda.Syntax.Strict</li>
                        <li>
                          <i>Translation</i>
                          <ul class="modules">
                            <li>Agda.Syntax.Translation.AbstractToConcrete</li>
                            <li>Agda.Syntax.Translation.ConcreteToAbstract</li>
                            <li>Agda.Syntax.Translation.InternalToAbstract</li>
                          </ul>
                        </li>
                      </ul>
                    </li>
                    <li>
                      <i>Termination</i>
                      <ul class="modules">
                        <li>Agda.Termination.CallGraph</li>
                        <li>Agda.Termination.Lexicographic</li>
                        <li>Agda.Termination.Matrix</li>
                        <li>Agda.Termination.Semiring</li>
                        <li>Agda.Termination.TermCheck</li>
                        <li>Agda.Termination.Termination</li>
                      </ul>
                    </li>
                    <li>Agda.Tests</li>
                    <li>Agda.TypeChecker</li>
                    <li>
                      <i>TypeChecking</i>
                      <ul class="modules">
                        <li>Agda.TypeChecking.Abstract</li>
                        <li>Agda.TypeChecking.Constraints</li>
                        <li>Agda.TypeChecking.Conversion</li>
                        <li>Agda.TypeChecking.Coverage
                          <ul class="modules">
                            <li>Agda.TypeChecking.Coverage.Match</li>
                          </ul>
                        </li>
                        <li>Agda.TypeChecking.DisplayForm</li>
                        <li>Agda.TypeChecking.Empty</li>
                        <li>Agda.TypeChecking.Errors</li>
                        <li>Agda.TypeChecking.EtaContract</li>
                        <li>Agda.TypeChecking.Free</li>
                        <li>Agda.TypeChecking.Implicit</li>
                        <li>Agda.TypeChecking.Injectivity</li>
                        <li>Agda.TypeChecking.Level</li>
                        <li>Agda.TypeChecking.MetaVars
                          <ul class="modules">
                            <li>Agda.TypeChecking.MetaVars.Occurs</li>
                          </ul>
                        </li>
                        <li>Agda.TypeChecking.Monad
                          <ul class="modules">
                            <li>Agda.TypeChecking.Monad.Base</li>
                            <li>Agda.TypeChecking.Monad.Builtin</li>
                            <li>Agda.TypeChecking.Monad.Closure</li>
                            <li>Agda.TypeChecking.Monad.Constraints</li>
                            <li>Agda.TypeChecking.Monad.Context</li>
                            <li>Agda.TypeChecking.Monad.Debug</li>
                            <li>Agda.TypeChecking.Monad.Env</li>
                            <li>Agda.TypeChecking.Monad.Exception</li>
                            <li>Agda.TypeChecking.Monad.Imports</li>
                            <li>Agda.TypeChecking.Monad.MetaVars</li>
                            <li>Agda.TypeChecking.Monad.Mutual</li>
                            <li>Agda.TypeChecking.Monad.Open</li>
                            <li>Agda.TypeChecking.Monad.Options</li>
                            <li>Agda.TypeChecking.Monad.Signature</li>
                            <li>Agda.TypeChecking.Monad.SizedTypes</li>
                            <li>Agda.TypeChecking.Monad.State</li>
                            <li>Agda.TypeChecking.Monad.Statistics</li>
                            <li>Agda.TypeChecking.Monad.Trace</li>
                          </ul>
                        </li>
                        <li>
                          <i>Patterns</i>
                          <ul class="modules">
                            <li>Agda.TypeChecking.Patterns.Match</li>
                          </ul>
                        </li>
                        <li>Agda.TypeChecking.Polarity</li>
                        <li>Agda.TypeChecking.Positivity</li>
                        <li>Agda.TypeChecking.Pretty</li>
                        <li>Agda.TypeChecking.Primitive</li>
                        <li>Agda.TypeChecking.Rebind</li>
                        <li>Agda.TypeChecking.Records</li>
                        <li>Agda.TypeChecking.Reduce</li>
                        <li>
                          <i>Rules</i>
                          <ul class="modules">
                            <li>Agda.TypeChecking.Rules.Builtin</li>
                            <li>Agda.TypeChecking.Rules.Data</li>
                            <li>Agda.TypeChecking.Rules.Decl</li>
                            <li>Agda.TypeChecking.Rules.Def</li>
                            <li>Agda.TypeChecking.Rules.LHS
                              <ul class="modules">
                                <li>Agda.TypeChecking.Rules.LHS.Implicit</li>
                                <li>Agda.TypeChecking.Rules.LHS.Instantiate</li>
                                <li>Agda.TypeChecking.Rules.LHS.Problem</li>
                                <li>Agda.TypeChecking.Rules.LHS.Split</li>
                                <li>Agda.TypeChecking.Rules.LHS.Unify</li>
                              </ul>
                            </li>
                            <li>Agda.TypeChecking.Rules.Record</li>
                            <li>Agda.TypeChecking.Rules.Term</li>
                          </ul>
                        </li>
                        <li>Agda.TypeChecking.Serialise</li>
                        <li>Agda.TypeChecking.SizedTypes</li>
                        <li>Agda.TypeChecking.Substitute</li>
                        <li>Agda.TypeChecking.Telescope</li>
                        <li>
                          <i>Test</i>
                          <ul class="modules">
                            <li>Agda.TypeChecking.Test.Generators</li>
                          </ul>
                        </li>
                        <li>Agda.TypeChecking.Tests</li>
                        <li>Agda.TypeChecking.With</li>
                      </ul>
                    </li>
                    <li>
                      <i>Utils</i>
                      <ul class="modules">
                        <li>Agda.Utils.Char</li>
                        <li>Agda.Utils.Either</li>
                        <li>Agda.Utils.FileName</li>
                        <li>Agda.Utils.Fresh</li>
                        <li>Agda.Utils.Function</li>
                        <li>Agda.Utils.Generics</li>
                        <li>Agda.Utils.Graph</li>
                        <li>Agda.Utils.Hash</li>
                        <li>
                          <i>IO</i>
                          <ul class="modules">
                            <li>Agda.Utils.IO.Binary</li>
                            <li>Agda.Utils.IO.Locale</li>
                            <li>Agda.Utils.IO.UTF8</li>
                          </ul>
                        </li>
                        <li>Agda.Utils.Impossible</li>
                        <li>Agda.Utils.List</li>
                        <li>Agda.Utils.Map</li>
                        <li>Agda.Utils.Maybe</li>
                        <li>Agda.Utils.Monad</li>
                        <li>Agda.Utils.Permutation</li>
                        <li>Agda.Utils.Pointer</li>
                        <li>Agda.Utils.Pretty</li>
                        <li>Agda.Utils.QuickCheck</li>
                        <li>Agda.Utils.ReadP</li>
                        <li>Agda.Utils.SemiRing</li>
                        <li>Agda.Utils.Size</li>
                        <li>Agda.Utils.String</li>
                        <li>Agda.Utils.Suffix</li>
                        <li>Agda.Utils.TestHelpers</li>
                        <li>Agda.Utils.Trace</li>
                        <li>Agda.Utils.Trie</li>
                        <li>Agda.Utils.Tuple</li>
                        <li>Agda.Utils.Unicode</li>
                        <li>Agda.Utils.Warshall</li>
                      </ul>
                    </li>
                    <li>Agda.Version</li>
                  </ul>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
      <div class="embedded-author-content">
        <h1>The Cabal library package</h1>
        <p>See the <a href="http://www.haskell.org/cabal/">Cabal web site</a> for more information.</p>
        <p>If you also want the <code>cabal</code> command-line program, you need the<a href="../cabal-install/README.md">cabal-install</a> package in addition to this library.</p>
        <h1>Installing the Cabal library</h1>
        <h2>If you already have the <code>cabal</code> program</h2>
        <p>In this case run:</p>
          <pre><code>cabal install</code></pre>
        <p>However, if you do not have an existing version of the <code>cabal</code> program, you first must install the Cabal library. To avoid this bootstrapping problem, you can install the Cabal library directly as described below.</p>
        <h2>Installing as a user (no root or administrator access)</h2>
          <pre><code>ghc -threaded --make Setup./Setup configure --user./Setup build./Setup install</code></pre>
        <p>Note the use of the <code>--user</code> flag at the configure step.</p>
        <p>Compiling 'Setup' rather than using <code>runghc Setup</code> is much faster and works on Windows. For all packages other than Cabal itself, it is fine to use <code>runghc</code>.</p>
        <p>This will install into <code>\$HOME/.cabal/</code> on Unix and into<code>Documents and Settings\$User\Application Data\cabal\</code> on Windows. If you want to install elsewhere, use the <code>--prefix=</code> flag at the configure step.</p>
        <h2>Installing as root or Administrator</h2>
          <pre><code>ghc -threaded --make Setup./Setup configure./Setup buildsudo ./Setup install</code></pre>
        <p>Compiling Setup rather than using <code>runghc Setup</code> is much faster and works on Windows. For all packages other than Cabal itself, it is fine to use <code>runghc</code>.</p>
        <p>This will install into <code>/usr/local</code> on Unix, and on Windows it will install into <code>\$ProgramFiles/Haskell</code>. If you want to install elsewhere, use the
          <code>--prefix=</code> flag at the configure step.</p>
        <h1>Using older versions of GHC and Cabal</h1>
        <p>It is recommended that you leave any pre-existing version of Cabal installed. In particular, it is <em>essential</em> you keep the version that came with GHC itself, since other installed packages require it (for instance, the "ghc" API package).</p>
        <p>Prior to GHC 6.4.2, however, GHC did not deal particularly well with having multiple versions of packages installed at once. So if you are using GHC 6.4.1 or older and you have an older version of Cabal installed, you should probably remove it by running:</p>
          <pre><code>\$ ghc-pkg unregister Cabal</code></pre>
        <p>or, if you had Cabal installed only for your user account, run:</p>
          <pre><code>\$ ghc-pkg unregister Cabal --user</code></pre><h1>The <code>filepath</code> dependency</h1>
        <p>Cabal uses the <a href="http://hackage.haskell.org/package/filepath">filepath</a> package, so it must be installed first. GHC version 6.6.1 and later come with <code>filepath</code>, however, earlier versions do not by default. If you do not already have
          <code>filepath</code>, you need to install it. You can use any existing version of Cabal to do that. If you have neither Cabal nor
          <code>filepath</code>, it is slightly harder but still possible.</p>
        <p>Unpack Cabal and <code>filepath</code> into separate directories. For example:</p>
          <pre><code>tar -xzf filepath-1.1.0.0.tar.gztar -xzf Cabal-1.6.0.0.tar.gz# rename to make the following instructions simpler:mv filepath-1.1.0.0/ filepath/mv Cabal-1.6.0.0/ Cabal/cd Cabalghc -i../filepath -cpp --make Setup.hs -o ../filepath/setupcd ../filepath/./setup configure --user./setup build./setup install</code></pre>
        <p>This installs <code>filepath</code> so that you can install Cabal with the normal method.</p>
        <h1>More information</h1>
        <p>Please see the <a href="http://www.haskell.org/cabal/">Cabal web site</a> for the <a href="http://www.haskell.org/cabal/users-guide">user guide</a> and <a href="http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html">APIdocumentation</a>. There is additional information available on the
          <a href="https://github.com/haskell/cabal/wiki">development wiki</a>.</p>
        <h1>Bugs</h1>
        <p>Please report bugs and feature requests to Cabal's <a href="https://github.com/haskell/cabal/issues">bug tracker</a>.</p>
        <h2>Your help</h2>
        <p>To help Cabal's development, it is enormously helpful to know from Cabal's users what their most pressing problems are with Cabal and<a href="http://hackage.haskell.org">Hackage</a>. You may have a favourite Cabal bug or limitation. Look at Cabal's
          <a href="https://github.com/haskell/cabal/issues">bug tracker</a>. Ensure that the problem is reported there and adequately described. Comment on the issue to report how much of a problem the bug is for you. Subscribe to the issues's notifications to discussed requirements and keep informed on progress. For feature requests, it is helpful if there is a description of how you would expect to interact with the new feature.</p>
        <h1>Source code</h1>
        <p>You can get the master development branch using:</p>
          <pre><code>\$ git clone https://github.com/haskell/cabal.git</code></pre><h1>Credits</h1>
        <p>Cabal developers (in alphabetical order):</p>
        <ul>
          <li>Krasimir Angelov</li>
          <li>Bjorn Bringert</li>
          <li>Duncan Coutts</li>
          <li>Isaac Jones</li>
          <li>David Himmelstrup ("Lemmih")</li>
          <li>Simon Marlow</li>
          <li>Ross Patterson</li>
          <li>Thomas Schilling</li>
          <li>Martin Sjugren</li>
          <li>Malcolm Wallace</li>
          <li>and nearly 30 other people have contributed occasional patches</li>
        </ul>
        <p>Cabal specification authors:</p>
        <ul>
          <li>Isaac Jones</li>
          <li>Simon Marlow</li>
          <li>Ross Patterson</li>
          <li>Simon Peyton Jones</li>
          <li>Malcolm Wallace</li>
        </ul>
      </div>
    </div>
  </body>
</html>
