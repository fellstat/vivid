var currentTab;

$(function () {

  //when ever any tab is clicked this method will be call
  $("#doc-tabs").on("click", "a", function (e) {
    e.preventDefault();
    Shiny.setInputValue("active_doc",$(this).attr("href"));
  });

  $("#doc-tabs").on("click", "button", function (e) {
    console.log("close");
    var doc_id = $(this).parent().attr("href");
    Shiny.setInputValue("close_doc",doc_id);
  });
});

