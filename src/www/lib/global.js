$(document).ready(function() {
  // collapse-item-header被点击时
  $(document).on('click', '.collapse-item-header', function() {
    // 切换当前点击的collapse-item-header元素同级的collapse-item-body的显示状态
    $(this).siblings('.collapse-item-body').slideToggle();

    // 找到当前点击的 .collapse-item-header 元素的子元素
    var children = $(this).children();

    // 在找到的子元素中，切换所有 class 为 .btn-right 的元素的显示/隐藏状态
    children.find('.collapse-btn-arrow-right').toggle();

    // 在找到的子元素中，切换所有 class 为 .btn-down 的元素的显示/隐藏状态
    children.find('.collapse-btn-arrow-down').toggle();
  });

  $('.collapse-subitem .collapse-item-header').click(function() {
    // 找到当前点击的 .collapse-item-header 元素最近的父级 .collapse-subitem 下的 .collapse-subitem-body
    var body = $(this).closest('.collapse-subitem').find('.collapse-subitem-body');
    // 切换 .collapse-subitem-body 的显示/隐藏状态
    body.slideToggle();
  });
});