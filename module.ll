; ModuleID = 'basic'
source_filename = "<string>"

declare i32 @external_function(i32)

define i32 @f() {
entry:
  %0 = call i32 @external_function(i32 21)
  ret i32 %0
}
