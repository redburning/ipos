user_base <- function() {
  con <-  RSQLite::dbConnect(RSQLite::SQLite(), "data/ipos.db")
  base <- con %>% dplyr::tbl("users") %>% as.data.frame()
  base <- base[c("user", "name", "org", "password", "permissions", "lock", "expired_time")]
  base[["lock"]] <- gsub("0", "正常", base[["lock"]])
  base[["lock"]] <- gsub("1", "锁定", base[["lock"]])
  base[["permissions"]] <- gsub("admin", "管理员用户", base[["permissions"]])
  base[["permissions"]] <- gsub("standard", "普通用户", base[["permissions"]])
  return(base)
}

userManagementUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    hr(),
    br(), br(),
    fluidRow(
      actionButton(inputId = ns("adduser"), label = "新增", icon = icon("plus"), style = "background-color: #28b5b5; color: #ffffff; border: none; margin-left: 15px; margin-bottom: 15px"),
      uiOutput(outputId = ns("edituser"), inline = TRUE),
      uiOutput(outputId = ns("deleteuser"), inline = TRUE)
    ),
    reactableOutput(ns("user_table"))
  )
}

userManagementServer <- function(input, output, session) {
  ns <- session$ns
  
  user_data <- user_base()
  
  selected <- reactive(getReactableState("user_table", "selected"))
  
  output$user_table <- renderReactable({
    reactable(data = user_data,
              bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20, selection = "multiple", onClick = "select", 
              columns = list(
                .selection = colDef(
                  width = 80,
                  # sticky = "left",
                  style = list(cursor = "pointer"),
                  headerStyle = list(cursor = "pointer")
                )
              )
              )
    
  })
  
  observeEvent(input$adduser, {
    showModal(modalDialog(
      title = "新建用户",
      size = "m",
      fluidPage(
        fluidRow(
          column(width = 12, textInput(inputId = ns("user"), label = "用户名"))
        ),
        fluidRow(
          column(width = 12, textInput(inputId = ns("name"), label = "姓名"))
        ),
        fluidRow(
          column(width = 12, textInput(inputId = ns("org"), label = "组织机构"))
        ),
        fluidRow(
          column(width = 12, passwordInput(inputId = ns("password"), label = "密码"))
        ),
        fluidRow(
          column(width = 12, selectInput(inputId = ns("permissions"), label = "角色", choices = c("普通用户" = "standard", "管理员用户" = "admin")))
        ),
        fluidRow(
          column(width = 12, dateInput(inputId = ns("expired_time"), label = "账号有效期至", value = ""))
        )
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = ns("useradd_confirm"), label = "确认"),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$useradd_confirm, {
    tryCatch({
      if (input$user == "") {
        toastr_warning(title = "保存失败", message = "用户名不能为空!")
      } else if (input$password == "") {
        toastr_warning(title = "保存失败", message = "密码不能为空!")
      } else {
        con <-  RSQLite::dbConnect(RSQLite::SQLite(), "data/ipos.db")
        
        addusersql <- paste0("INSERT INTO users (user, name, org, password, password_hash, permissions, lock, expired_time) 
                           VALUES ", "('", input$user, "', '", input$name, "', '", input$org, "', '", input$password, "', '", sodium::password_store(input$password), "', '", input$permissions, "', ' 0', '", input$expired_time, "')")
        cat(addusersql)
        RSQLite::dbExecute(con, addusersql)
        # refresh table instance
        user_data <<- user_base()
        
        output$user_table <- renderReactable({
          reactable(data = user_data,
                    bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20, selection = "multiple", onClick = "select", 
                    columns = list(
                      .selection = colDef(
                        width = 80,
                        # sticky = "left",
                        style = list(cursor = "pointer"),
                        headerStyle = list(cursor = "pointer")
                      )
                    )
          )
          
        })
        toastr_success(title = "保存成功", message = "")
        removeModal()
      }
    },
    warning = function(w) {
      toastr_error("保存失败", message = w)
    },
    error = function(e) {
      toastr_error("保存失败", message = e)
    })
    
  })
  
  # 编辑
  output$edituser <- renderUI({
    if (length(selected()) == 1) {
      actionButton(inputId = ns("edituser"), label = "编辑", icon = icon("edit"), style = "background-color: #28b5b5; color: #ffffff; border: none; margin-left: 15px; margin-bottom: 15px")
    } else {
      shinyjs::disabled(actionButton(inputId = ns("edituser"), label = "编辑", icon = icon("edit"), style = "background-color: #28b5b5; color: #ffffff; border: none; margin-left: 15px; margin-bottom: 15px"))
    }
  })
  
  observeEvent(input$edituser, {
    # 管理员用户只能修改密码
    if (1 %in% selected()) {
      showModal(modalDialog(
        title = "修改信息",
        size = "m",
        fluidPage(
          fluidRow(
            column(width = 12, passwordInput(inputId = ns("edit_admin_password_old"), label = "原始密码")),
            column(width = 12, passwordInput(inputId = ns("edit_admin_password_new1"), label = "新密码")),
            column(width = 12, passwordInput(inputId = ns("edit_admin_password_new2"), label = "重新输入新密码"))
          )
        ),
        easyClose = FALSE,
        fade = FALSE,
        footer = tagList(
          actionButton(inputId = ns("useredit_admin_confirm"), label = "确认"),
          modalButton(label = "取消")
        )
      ))
    } else {
      showModal(modalDialog(
        title = "修改信息",
        size = "m",
        fluidPage(
          fluidRow(
            column(width = 12, textInput(inputId = ns("edit_standard_name"), label = "姓名", value = user_data[selected(), "name"]))
          ),
          fluidRow(
            column(width = 12, textInput(inputId = ns("edit_standard_org"), label = "组织机构", value = user_data[selected(), "org"]))
          ),
          fluidRow(
            column(width = 12, passwordInput(inputId = ns("edit_standard_password"), label = "密码", value = user_data[selected(), "password"]))
          ),
          fluidRow(
            column(width = 12, selectInput(inputId = ns("edit_standard_lockstate"), label = "账号状态", choices = c("正常" = "0", "锁定" = "1"), 
                                           selected = ifelse(user_data[selected(), "lock"]=="0", "0", "1")))
          ),
          fluidRow(
            column(width = 12, selectInput(inputId = ns("edit_standard_permissions"), label = "角色", choices = c("普通用户" = "standard", "管理员用户" = "admin"), 
                                           selected = ifelse(user_data[selected(), "permissions"]=="admin", "admin", "standard")))
          ),
          fluidRow(
            column(width = 12, dateInput(inputId = ns("edit_standard_expired_time"), label = "账号有效期至", value = user_data[selected(), "expired_time"]))
          ),
        ),
        easyClose = FALSE,
        fade = FALSE,
        footer = tagList(
          actionButton(inputId = ns("useredit_standard_confirm"), label = "确认"),
          modalButton(label = "取消")
        )
      ))
    }
    
  })
  
  observeEvent(input$useredit_admin_confirm, {
    # 更新管理员密码
    if (input$edit_admin_password_old != user_data[selected(), "password"]) {
      toastr_warning(title = "修改失败", message = "原始密码输入错误!")
    } else {
      if (input$edit_admin_password_new1 != input$edit_admin_password_new2) {
        toastr_warning(title = "修改失败", message = "新密码输入不一致!")
      } else if (input$edit_admin_password_new1 == "" || input$edit_admin_password_new2 == "") {
        toastr_warning(title = "修改失败", message = "新密码不能为空!")
      } else {
        tryCatch({
          editadminsql <- paste0("UPDATE users set password=", "'", input$edit_admin_password_new2, "'", 
                                 ", password_hash=", "'", sodium::password_store(input$edit_admin_password_new2), "'", 
                                 " WHERE user='admin'")
          cat(editadminsql)
          con <-  RSQLite::dbConnect(RSQLite::SQLite(), "data/ipos.db")
          RSQLite::dbExecute(con, editadminsql)
          
          # refresh table instance
          user_data <<- user_base()
          
          output$user_table <- renderReactable({
            reactable(data = user_data,
                      bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20, selection = "multiple", onClick = "select", 
                      columns = list(
                        .selection = colDef(
                          width = 80,
                          # sticky = "left",
                          style = list(cursor = "pointer"),
                          headerStyle = list(cursor = "pointer")
                        )
                      )
            )
          })
          toastr_success(title = "修改成功", message = "")
          removeModal()
        },
        error = function(cond) {
          toastr_error("修改失败", message = cond)
        })
      }
    }
    
  })
  
  observeEvent(input$useredit_standard_confirm, {
    # 更新普通用户信息
    if (input$edit_standard_password == "") {
      toastr_warning(title = "修改失败", message = "用户密码不能为空!")
    } else {
      tryCatch({
        edit_standard_user_sql <- paste0("UPDATE users set name=", "'", input$edit_standard_name, "'",
                                         ", org=", "'", input$edit_standard_org, "'", 
                                         ", password=", "'", input$edit_standard_password, "'", 
                                         ", password_hash=", "'", sodium::password_store(input$edit_standard_password), "'", 
                                         ", permissions=", "'", input$edit_standard_permissions, "'",
                                         ", lock=", "'", input$edit_standard_lockstate, "'",
                                         ", expired_time=", "'", input$edit_standard_expired_time, "'", 
                                         " WHERE user=", "'", user_data[selected(), "user"], "'")
        cat(edit_standard_user_sql)
        
        con <-  RSQLite::dbConnect(RSQLite::SQLite(), "data/ipos.db")
        
        RSQLite::dbExecute(con, edit_standard_user_sql)
        # refresh table instance
        user_data <<- user_base()
        
        output$user_table <- renderReactable({
          reactable(data = user_data,
                    bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20, selection = "multiple", onClick = "select", 
                    columns = list(
                      .selection = colDef(
                        width = 80,
                        # sticky = "left",
                        style = list(cursor = "pointer"),
                        headerStyle = list(cursor = "pointer")
                      )
                    )
          )
        })
        toastr_success(title = "修改成功", message = "")
        removeModal()
      },
      error = function(cond) {
        toastr_error("修改失败", message = cond)
      })
    }
  })
  
  # 删除
  output$deleteuser <- renderUI({
    if (length(selected()) > 0 && !(1 %in% selected())) {
      actionButton(inputId = ns("deleteuser"), label = "删除", icon = icon("trash-alt"), style = "background-color: #D9534F; color: #ffffff; border: none; margin-left: 15px; margin-bottom: 15px")
    } else {
      shinyjs::disabled(actionButton(inputId = ns("deleteuser"), label = "删除", icon = icon("trash-alt"), style = "background-color: #D9534F; color: #ffffff; border: none; margin-left: 15px; margin-bottom: 15px"))
    }
  })
  
  observeEvent(input$deleteuser, {
    showModal(modalDialog(
      title = "删除用户",
      size = "m",
      fluidPage(
        span(paste0("确定删除用户 ", paste(user_data[selected(), "user"], collapse = ", "), " 吗 ?"))
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = ns("userdelete_confirm"), label = "确认"),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$userdelete_confirm, {
    tryCatch({
      con <-  RSQLite::dbConnect(RSQLite::SQLite(), "data/ipos.db")
      deleteSql <- paste0("DELETE FROM users WHERE user=", "'", user_data[selected(), "user"], "'")
      for (sql in deleteSql) {
        RSQLite::dbExecute(con, sql)
      }
      # refresh table instance
      user_data <<- user_base()
      
      output$user_table <- renderReactable({
        reactable(data = user_data,
                  bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20, selection = "multiple", onClick = "select", 
                  columns = list(
                    .selection = colDef(
                      width = 80,
                      # sticky = "left",
                      style = list(cursor = "pointer"),
                      headerStyle = list(cursor = "pointer")
                    )
                  )
        )
      })
      toastr_success(title = "删除成功", message = "")
      removeModal()
    },
    error = function(cond) {
      toastr_error("删除失败", message = cond)
    })
  })
}