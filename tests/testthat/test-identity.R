test_that("name and email are split from name_email strings", {
  require(stringi)
  split_format_name_email <- function(x) split_name_email(format_name_email(x))
  expect_equal(split_format_name_email("Firstname Lastname <emailatxxx.org>"),
               list(name = "Firstname Lastname",email= "email@xxx.org",id = NA))
  expect_equal(split_format_name_email("Gerd Dixx <chzyu-H+wXaHxf7aLQT0dZR+AgaA-AT-public.gmane.org>"),
               list(name = "Gerd Dixx",email= "chzyu-H+wXaHxf7aLQT0dZR+AgaA-@-public.gmane.org",id = NA))
  expect_equal(split_format_name_email("Firstname <email at xxx.org>"),
               list(name = "Firstname",email= "email@xxx.org",id = NA))
  expect_equal(split_format_name_email("info@mob-xxx.com"),
               list(name = NA, email= "info@mob-xxx.com",id = NA))
  expect_equal(split_format_name_email("Dezd-Wallwa van Gx"),
               list(name = "Dezd-Wallwa van Gx", email= NA,id = NA))
  expect_equal(split_format_name_email('r...@xxx.org ("Zdzl, Rwzy via RT")'),
               list(name = "Zdzl Rwzy", email= 'r...@xxx.org',id = NA))
})
test_that("name and email are formatted", {
  expect_equal(format_name_email("Firstname Lastname <emailatxxx.org>"),
               "Firstname Lastname email@xxx.org")
  expect_equal(format_name_email("Gerd Dixx <chzyu-H+wXaHxf7aLQT0dZR+AgaA-AT-public.gmane.org>"),
               "Gerd Dixx chzyu-H+wXaHxf7aLQT0dZR+AgaA-@-public.gmane.org")
  expect_equal(format_name_email("Firstname <email at xxx.org>"),
               "Firstname email@xxx.org")
  expect_equal(format_name_email("info@mob-xxx.com"),
               "info@mob-xxx.com")
  expect_equal(format_name_email("Dezd-Wallwa van Gx"),
               "Dezd-Wallwa van Gx")
  expect_equal(format_name_email('r...@xxx.org ("Zdzl, Rwzy via RT")'),
               'r...@xxx.org Zdzl Rwzy')
  #"r...@openssl.org M@t Caswell
  expect_equal(format_name_email('r...@open.org ("Matt Zwx via RT")'),
               "r...@open.org Matt Zwx")
})
test_that("pair of identities are the same for the same person",{
  require(stringi)
  split_format_name_email <- function(x) split_name_email(format_name_email(x))
  expect_equal(is_same_identity(
    1,
    2,
    list(split_format_name_email(list(name = "Rwzy Zdzl", email= "rt@open.org",id = NA)),
      split_format_name_email(list(name = "Zdzl Rwzy", email= "rzdzl@zzz.com",id = NA))
    )
  ),
  TRUE)
  expect_equal(is_same_identity(
    1,
    2,
    list(split_format_name_email(list(name = "Rwzy Zdzl", email= "rt@open.org",id = NA)),
         split_format_name_email(list(name = "Zdzl Rwzy", email= "rzdzl@zzz.com",id = NA))
    )
  ),
  TRUE)
})
test_that("all identities are the same for the same person",{
  expect_equal(assign_exact_identity(
    c(
      "rt at open.org (Rwzy Zdzl via RT)",
      "rzdzl at open.org (Rwzy Zdzl)"
    )),
    c(1,1))
  expect_equal(assign_exact_identity(
    c(
      "rt at open.org (Rwzy Zdzl via RT)",
      "rzdzl at zzz.com (Zdzl, Rwzy)"
    )),
    c(1,1))
  expect_equal(assign_exact_identity(
    c(
      "rt at open.org (Rwzy Zdzl via RT)",
      "Rwzy Zdzl <rzdzl@open.org>",
      "Rwzy Zdzl <rzdzl@zzz.com>",
      "rzdzl at zzz.com (Zdzl, Rich)",
      "rzdzl at open.org (Rwzy Zdzl)"
      )),
    c(1, 1, 1, 1, 1))
  expect_equal(assign_exact_identity(
    c(
      "zxc at open.org (Ay Vkl)",
      "rt at open.org (Ay Vkl via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "rt at open.org (Jwe Esa via RT)",
      "jwe at open.org (Jwe Esa)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "Jnh.Mvc at ft.com (Jnh Mvc)",
      "rt at open.org (Jnh Mvc via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "jfg at wu.org (Jfg Wu)",
      "rt at open.org (Jfg Wu via RT)",
      "Jfg Wu <wu@open.org>")),
    c(1, 1, 1))
  expect_equal(assign_exact_identity(
    c(
      "ttwu at gmail.com (Kyh Zuea)",
      "ttwu at gmail.com ( Kyh Züa )")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "xkjh at domain.sjk (Xkjh Chj)",
      "rt at open.org (Xkjh Chj via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "kjui at ziuy.org (Ko Jui)",
      "rt at open.org (Ko Jui via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "udi at nos.ud (Aiok Oiu)",
      "rt at open.org (Aiok Oiu via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "rt at open.org (Blue, Kare via RT)",
      "kblue at www.com (Blue, Kare)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "zoiu at somedomain.com (Zowa Oiu)",
      "rt at open.org (Zowa Oiu via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "xkh at somedomain.com (Xkh Uio)",
      "rt at open.org (Xkh Uio via RT)")),
    c(1, 1))
  expect_equal(assign_exact_identity(
    c(
      "MAILER-DAEMON at open.org (The Post Office)",
      "postmaster at open.org (The Post Office)",
      "MAILER-DAEMON at open.org (Bounced mail)",
      "postmaster at open.org (Post Office)")),
    c(1,1,1,1))
  expect_equal(assign_exact_identity(
    c(
      "jhgnst at jjj.com ( Jhg Nst (振) )",
      "jhgnst at jjj.com ( Jhg Nst (振) )")),
    c(1, 1))
  # All tests after this block are skipped
  skip("Advanced cases which do not affect current dataset.")
  # Only First Name Match
  expect_equal(assign_exact_identity(
    c(
      'u.l...@info.ha-gade.tu ("Uler Yoko")',
      'u.l...@info.ha-gade.tu (Uler)'),
    use_name_only=TRUE),
    c(1, 1))
  # Identity of 1st and 2nd will match by name if (xxx) and via RT is not taken into account
  # All identities will end up the same
  expect_equal(assign_exact_identity(
    c(
      "xxx at uy.com (Wy Zkk (zkkw))",
      "rt at open.org (Wy Zkk via RT)",
      "xxx at uy.com (Wy Zkk)")),
    c(1, 1, 1))
  # Identities of 1st and 2nd will NOT match by name 3rd would need to be matched first.
  # This is a case that only applies to assign exact identity, because the order of
  # matching is what would dictate success.
  # Calling the function a few additional times until it can't detect any decrease on
  # the number of IDs would be a solution. Considering we use a variant of insertion
  # sort, this will be ~ O(n) as we largely approximate the best case scenario after
  # the first execution (?).
  expect_equal(assign_exact_identity(
    c(
      "xxx at uy.com (Some Name)",
      "rt at open.org (Wy Zkk via RT)",
      "xxx at uy.com (Wy Zkk)")),
    c(1, 1, 1))
})
