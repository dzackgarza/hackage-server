<!DOCTYPE html>
<html>

  <head>
    $hackageCssTheme()$
    <title>$pkgName$ | Hackage</title>
  </head>

  <body>

    $header()$

    <div id="content">

      <div class="content-wrap">

        <div id="content-header">
          <h2 id="package-name">$pkgName$</h2>

          <div id="version-info">
          $versions$
          <!--
            <ul>
              <li id="current-version">2.2.6</li>
              <li>2.2.4</li>
              <li>2.2.2</li>
              <li>2.2.0</li>
              <li>...</li>
            </ul>
            -->
            <span>
              <a href = "#">View All Versions</a>
              <a href> >></a>
            </span>
          </div>

          <span id="current-version">
            Current Version: 2.2.8
          </span>
          <br>

          <span id="last-update">
            Uploaded: $uploadTime$
            <br>
            Votes: $votes$
          </span>
          <br>

          <br>

        </div> <!-- /content-header -->
        <div id="content-body">

          <p id="synopsis">
            A dependently typed functional programming language and proof assistant.
          </p>
          <a id="skip-to-readme" href="#readme">[Skip to ReadMe]</a>

          <div id="tags" style="margin-bottom: 20px">
            <h3>Tags:</h3>
            <ul>
              <li class="system-tag">library</li>
              <li class="system-tag">program</li>
              <li class="system-tag">dependent-types</li>
            </ul>
          </div>

          <input type="checkbox" id="expand-desc" class="toggle-box checkmark">
          <label for="expand-desc" class="toggle-box">
            <span></span>
          </label>

          <div id="description" class="desc">
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
          </div> <!-- /description -->

          <div id="more-information">

            <div id="modules">
              <div id="module-list">
                $moduleList$
              </div> <!-- /module-list -->
            </div> <!-- /modules -->

            <div id="dependencies">
              <h2>Dependencies</h2>
              $dependencyList$
            </div> <!-- /dependencies -->

          </div> <!-- /more-information -->

        </div> <!-- /content-body -->

      </div> <! -- /content-wrap -->

     <div class="sidebar">

      <div class="sidebar-box">
        <h3 id="install-header">Install</h3>

        <p id="license">License : $license$</p>

        <pre id="install-cabal"><code>&gt; cabal install $pkgName$</code></pre>

        <div class="clear-div"></div>

        <p id="install-distros">
          <span>Distributions</span>
          <br>
          $distributions$
        </p>

        <p id="install-other">
          <a href="#">Source Tarball</a><a href="#"> [Browse]</a> |
          <a href="#">Package Description</a> |
          <a href="#">Haddocks</a>
        </p>

      </div> <!-- /sidebar-box -->

      <div class="sidebar-box links">
        <h3 id="install-header">Links</h3>
        <ul id="information-links">
          <li><h4>Homepage</h4>
            <p>
              $homePage$
            </p>
          </li>
          <li>
            <h4>Bug Reports</h4>
            <p>
              $bugTracker$
            </p>
          </li>
          <li>
            <h4>Source Repository</h4>
            <p>
              $sourceRepo$
            </p>
          </li>
          <li>
            <hr>
          </li>
          <li>
            <h4>Categories</h4>
            <p>
              $categories$
            </p>
          </li>
          </ul>
      </div>

      <div class="sidebar-box properties">
        <h3>Properties</h3>
        <table>
          <tbody>
            <tr>
              <th>Changelog</th>
              <td><a href="#">CHANGELOG</a></td>
            </tr>
            <tr>
              <th>Copyright</th>
              <td>$copyright$</td>
            </tr>
            <tr>
              <th>Author(s)</th>
              <td>$author$</td>
            </tr>
            <tr>
              <th>Maintainer</th>
              <td>$maintainer$</td>
            </tr>
            <tr>
              <th>Stability</th>
              <td>$stability$</td>
            </tr>
          </tbody>
        </table>
      </div>

    </div> <!-- /sidebar -->

    <div id="readme-container">
      $readme$
    </div>

    </div> <!-- /content -->

  $footer()$

</body>
</html>
