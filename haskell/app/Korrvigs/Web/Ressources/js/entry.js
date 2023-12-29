$('.section-symbol').each(function () {
  var $symbol = $(this);
  $symbol.click(function () {
    $symbol.parent().parent().toggleClass('collapsed');
  });
});
