// 回车触发搜索
$(document).on('keypress', function(e) {
  if (e.which == 13) {  // 13 是 Enter 键的键码
    $('#pathway_gallery-search_btn').click();
  }
});

// 显示下拉列表
function showDropdown() {
  document.getElementById('pathway_gallery-search_history').style.display = 'block';
}

// 填充输入框的值
function selectValue(value) {
  const input = document.getElementById('pathway_gallery-search_input');
  input.value = value;
  Shiny.setInputValue('pathway_gallery-search_input', value);
  document.getElementById('pathway_gallery-search_history').style.display = 'none';
}

// 为 document 添加点击事件监听器以隐藏下拉列表
document.addEventListener('click', function(event) {
  const target = event.target;
  const input = document.getElementById('pathway_gallery-search_input');
  const history = document.getElementById('pathway_gallery-search_history');
  // 检查点击事件是否发生在 input 或下拉列表之外
  if (!input.contains(target) && !history.contains(target)) {
      history.style.display = 'none';
  }
  if (history.contains(target)) {
    $('#pathway_gallery-search_btn').click();  // 触发按钮点击事件
  }
});

$(document).ready(function() {
  // 为图片元素绑定点击事件
  $('body').on('click', '.pathway_gallery_img_container', function(event) {
    // 检查点击的是否是下载按钮或其子元素
    const target = event.target;
    // 假设下载按钮的类名为 'pathway_gallery_img_download'
    if ($(target).hasClass('pathway_gallery_img_download') || $(target).closest('.pathway_gallery_img_download').length) {
      // 处理下载逻辑
      handleDownload($(this));
    } else {
      // 处理图片点击逻辑
      handleImgClick($(this));
    }
  });

  // 关闭全屏图片
  $('.fullscreen_close_btn').on('click', function() {
    fullscreenDiv = document.getElementById('fullscreen');
    fullscreenDiv.style.display = 'none';
    // 恢复显示滚动条
    document.body.style.overflow = 'scroll';
  })

  // 为search history中的每个<li>元素添加点击事件
  $('#pathway_gallery-search_history').on('click', 'li', function() {
    // 获取触发事件的<li>元素的文本内容
    var itemText = $(this).text();
    selectValue(itemText)
  });

  $('#fullscreen-download').on('click', function() {
    handleDownload($(this));
  })
  
  // 点击logo显示default gallery
  $('#pathway-gallery-logo').click(function() {
      $('#none-search-result').hide();
      $('#pathway_gallery-search-result').hide();
      $('#default-gallery').css('display', 'flex');
  });
});

// 图片下载
function handleDownload(container) {
  // 获取图片的URL
  var src = container.data('imgSrc');
  // 从图片URL中提取文件名
  var filename = src.split('/').pop();

  // 创建一个临时的<a>元素
  var link = $('<a></a>', {
    href: src,
    download: filename,
    // 为了防止页面跳转，设置 display 为 none
    style: 'display:none'
  });

  // 模拟点击<a>元素
  $('body').append(link);
  link.get(0).click();

  // 点击后移除<a>元素
  link.remove();
}

// 图片点击全屏显示
function handleImgClick(container) {
  var src = container.data('imgSrc');
  var desc = container.data('imgDesc');
  var keywordsMetabolite = container.data('imgKeywordsM');
  var keywordsPathway = container.data('imgKeywordsP');
  var keywordsDisease = container.data('imgKeywordsD');
  var keywordsTarget = container.data('imgKeywordsT');
  var journal = container.data('imgJournal');
  var doi = container.data('imgDoi');

  fullscreenDiv = document.getElementById('fullscreen');
  fullscreenDiv.style.display = 'flex';
  fullscreenDiv.querySelector('img').src = src;
  document.getElementById('fullscreen-desc').innerHTML = desc;
  document.getElementById('fullscreen-journal').innerHTML = "文献出处: " + journal;
  document.getElementById('fullscreen-doi').innerHTML = "doi: " + "<a href='https://doi.org/" + doi + "' target='_blank'>" + doi + "</a>";
  document.getElementById('fullscreen-keywords-metabolite').innerHTML = keywordsMetabolite;
  document.getElementById('fullscreen-keywords-pathway').innerHTML = keywordsPathway;
  document.getElementById('fullscreen-keywords-disease').innerHTML = keywordsDisease;
  document.getElementById('fullscreen-keywords-target').innerHTML = keywordsTarget;
  
  // 更新下载按钮的data-img-src属性
  $('#fullscreen-download').data('imgSrc', src);
  // 全屏后隐藏滚动条
  document.body.style.overflow = 'hidden';
}
