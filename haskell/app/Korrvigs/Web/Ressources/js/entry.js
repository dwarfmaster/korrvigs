$('.fragment').each(function () {
  var $fieldset = $(this);
  var $legend = $fieldset.find('> legend');
  $legend.click(function () {
    $(this).parent().toggleClass('collapsed');
  });
});
