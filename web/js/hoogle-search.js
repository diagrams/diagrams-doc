var PACKAGES = '+diagrams-contrib +diagrams-lib +diagrams-core +diagrams-canvas \
+diagrams-cairo +diagrams-rasterific +diagrams-svg +diagrams-builder \
+diagrams-postscript +active +palette +diagrams-constraints'

$(document).ready(function(){
    $('#search').submit(function(event){
        window.location.href = 'http://haskell.org/hoogle?hoogle='
                             + encodeURIComponent($('#hoogle').val() + ' ' + PACKAGES);
        event.preventDefault();
    });
});
