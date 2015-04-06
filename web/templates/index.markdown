---
title: About diagrams
---
 <p class="lead" style="text-align: center;">
Diagrams is a powerful, flexible, declarative domain-specific language
for creating vector graphics, using the
[Haskell programming language](http://haskell.org/).
 </p>

 <div class="row">

 <div class="col-md-4">
 <div class="panel panel-default lead-panel" onclick="window.location='/doc/quickstart.html';" style="cursor: pointer;">
 <div class="panel-body">
 <p class="lead text-center">Get started</p>
 <p class="text-center">
Read the [quick start tutorial](/doc/quickstart.html) or the [user manual](http://projects.haskell.org/diagrams/doc/manual.html).
 </p>
 </div>
 </div>
 </div>

 <div class="col-md-4">
 <div class="panel panel-default" onclick="window.location='gallery.html';" style="cursor: pointer;">
 <div class="panel-body lead-panel">
 <p class="lead text-center">Get excited</p>
 <p class="text-center">
 Check out the [gallery](/gallery.html) for example images and
 code.
 </p>
 </div>
 </div>
 </div>

 <div class="col-md-4">
 <div class="panel panel-default" onclick="window.location='http://webchat.freenode.net/?channels=diagrams';" style="cursor: pointer;">
 <div class="panel-body lead-panel">
 <p class="lead text-center">Get connected</p>
 <p class="text-center">
Drop by the [`#diagrams` IRC channel](http://webchat.freenode.net/?channels=diagrams)
with questions, or post them to the
[mailing list](http://groups.google.com/group/diagrams-discuss).
 </p>
 </div>
 </div>
 </div>

 <form method="link" action="http://paste.hskll.org">
 <p class="lead text-center">
 Impatient? &nbsp;&nbsp; <button type="submit" value="clickable button" class="btn btn-primary btn-lg">Try diagrams in your browser!</button>
 </p>
 </form>

 <div class="row">
 <div class="col-md-8 col-md-offset-2">
News
----

 $for(news)$
 $partial("templates/newsitem.html")$
 $endfor$
 </div>
 </div>

