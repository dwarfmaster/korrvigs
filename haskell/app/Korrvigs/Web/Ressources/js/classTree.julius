$('#classes-tree .folder').each(function () {
  var $folder = $(this);
  $folder.click(function () {
    $(this).toggleClass('folded');
    if ($(this).hasClass('folded')) {
      $(this).text("▶");
    } else {
      $(this).text("▼");
    }
    $(this).parent().siblings('.nested').toggleClass('folded');
  });
});
