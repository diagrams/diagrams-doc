$('.panoramic').mousemove(function(e){
    var
        w = $(this).width();
        z = Math.min(Math.max((e.clientX-this.offsetLeft)/w, 0),1);
        x = -4000 * z;
    $(this).css('background-position-x', x + 'px ');
});