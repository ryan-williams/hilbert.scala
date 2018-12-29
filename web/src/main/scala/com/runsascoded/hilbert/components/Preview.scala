package com.runsascoded.hilbert.components

/**
 * State for components that support selection of one element at a time from a group, with different behaviors for
 * mouseovers of elements va. clicks
 *
 * @param hover Element currently being hovered over, if any
 * @param click Element currently "clicked", if any
 */
case class Preview[T](
  hover: Option[T] = None,
  click: Option[T] = None
) {
  def t: Option[T] = click.orElse(hover)
}
object Preview {
  implicit def unwrap[T](preview: Preview[T]): Option[T] = preview.t
}

