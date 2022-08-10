context("chart")

describe("chart", {

  it("produces a Chart object", {
    dtf <- data.frame(x = 1:2, y = 1:2)
    expect_is(chart(dtf, y ~ x), 'Chart')
  })
})
