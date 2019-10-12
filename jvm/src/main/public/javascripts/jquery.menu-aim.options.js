$('.dropdown-menu').menuAim({
    activate: function (row) {
        var $row = $(row),
            $submenu = $row.children('ul');

//        console.log('activated: ' + row.innerText);

        $row.children('a').addClass('hover');
        $submenu.css({display: 'block'});
    },

    deactivate: function(row) {
        var $row = $(row),
            $submenu = $row.children('ul');
        $row.find('a').removeClass('hover');
        $submenu.css('display', 'none');
    },

    exitMenu: function(){ return true; }
});