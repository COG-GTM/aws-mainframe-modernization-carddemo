import { blank } from './common';

export interface UserForm {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: string;
}

export type UserField = keyof UserForm;

export const EMPTY_USER_FORM: UserForm = { userId: '', firstName: '', lastName: '', password: '', userType: '' };

/** COUSR01C PROCESS-ENTER-KEY order: First Name, Last Name, User ID, Password, User Type. */
export function validateUserAdd(f: UserForm): { field: UserField; message: string } | null {
  if (blank(f.firstName)) return { field: 'firstName', message: 'First Name can NOT be empty...' };
  if (blank(f.lastName)) return { field: 'lastName', message: 'Last Name can NOT be empty...' };
  if (blank(f.userId)) return { field: 'userId', message: 'User ID can NOT be empty...' };
  if (blank(f.password)) return { field: 'password', message: 'Password can NOT be empty...' };
  return validateUserType(f.userType);
}

/** COUSR02C UPDATE-USER-INFO order: User ID, First Name, Last Name, User Type (password optional on PUT). */
export function validateUserUpdate(f: UserForm): { field: UserField; message: string } | null {
  if (blank(f.userId)) return { field: 'userId', message: 'User ID can NOT be empty...' };
  if (blank(f.firstName)) return { field: 'firstName', message: 'First Name can NOT be empty...' };
  if (blank(f.lastName)) return { field: 'lastName', message: 'Last Name can NOT be empty...' };
  return validateUserType(f.userType);
}

function validateUserType(userType: string): { field: UserField; message: string } | null {
  if (blank(userType)) return { field: 'userType', message: 'User Type can NOT be empty...' };
  if (!['A', 'U'].includes(userType.trim().toUpperCase())) {
    return { field: 'userType', message: 'User Type must be A (Admin) or U (User)...' };
  }
  return null;
}
