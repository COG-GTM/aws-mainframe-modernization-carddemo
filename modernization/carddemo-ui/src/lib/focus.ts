/** Cursor positioning (BMS `MOVE -1 TO <field>L`): focus the input named by ApiError.field (ids are API field paths). */
export function focusField(field: string | null | undefined): void {
  if (!field) return;
  window.requestAnimationFrame(() => {
    const exact = document.getElementById(field);
    const target =
      exact ??
      Array.from(document.querySelectorAll<HTMLElement>('input, select')).find(
        (el) => el.id.startsWith(`${field.replace(/\.$/, '')}.`) || el.id.startsWith(field),
      );
    if (target instanceof HTMLInputElement || target instanceof HTMLSelectElement) {
      target.focus();
      if (target instanceof HTMLInputElement && target.type !== 'radio') target.select();
    }
  });
}
