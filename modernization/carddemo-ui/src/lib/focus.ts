const ENTERABLE = 'input:not([readonly]):not([disabled]):not([type="hidden"]), select:not([disabled])';

function target(field: string | null | undefined): HTMLElement | undefined {
  if (!field) return document.querySelector<HTMLElement>(`.screen ${ENTERABLE.split(', ').join(', .screen ')}`) ?? undefined;
  return (
    document.getElementById(field) ??
    Array.from(document.querySelectorAll<HTMLElement>('input, select')).find(
      (el) => el.id.startsWith(`${field.replace(/\.$/, '')}.`) || el.id.startsWith(field),
    )
  );
}

function focusNow(field: string | null | undefined): boolean {
  const el = target(field);
  if (!(el instanceof HTMLInputElement || el instanceof HTMLSelectElement)) return false;
  el.focus();
  if (el instanceof HTMLInputElement && el.type !== 'radio') el.select();
  return document.activeElement === el;
}

/**
 * Cursor positioning (BMS `MOVE -1 TO <field>L`): focus the input named by ApiError.field (ids are API field paths);
 * without a field, the first enterable field of the screen, as the COBOL programs do for errors not tied to a field.
 */
export function focusField(field: string | null | undefined): void {
  window.requestAnimationFrame(() => {
    if (!focusNow(field)) window.setTimeout(() => focusNow(field), 50);
  });
}
