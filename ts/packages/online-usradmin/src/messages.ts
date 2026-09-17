/**
 * The message texts the COUSR programs move into `WS-MESSAGE`, byte for byte.
 */

/** `CCDA-MSG-INVALID-KEY` from `CSMSG01Y`. */
export const invalidKeyMessage = "Invalid key pressed. Please see below...         ";

export const messages = {
  firstNameEmpty: "First Name can NOT be empty...",
  lastNameEmpty: "Last Name can NOT be empty...",
  userIdEmpty: "User ID can NOT be empty...",
  passwordEmpty: "Password can NOT be empty...",
  userTypeEmpty: "User Type can NOT be empty...",
  userIdAlreadyExists: "User ID already exist...",
  userIdNotFound: "User ID NOT found...",
  unableToAddUser: "Unable to Add User...",
  unableToUpdateUser: "Unable to Update User...",
  unableToLookupUser: "Unable to lookup User...",
  pressPf5ToSave: "Press PF5 key to save your updates ...",
  pressPf5ToDelete: "Press PF5 key to delete this user ...",
  modifyToUpdate: "Please modify to update ...",
  invalidSelection: "Invalid selection. Valid values are U and D",
  alreadyTopOfPage: "You are already at the top of the page...",
  alreadyBottomOfPage: "You are already at the bottom of the page...",
  topOfPage: "You are at the top of the page...",
  reachedBottomOfPage: "You have reached the bottom of the page...",
  reachedTopOfPage: "You have reached the top of the page...",
} as const;

/** `STRING 'User ' SEC-USR-ID DELIMITED BY SPACE ' has been added ...'`. */
export const userAddedMessage = (userId: string): string =>
  `User ${userId.trim()} has been added ...`;

export const userUpdatedMessage = (userId: string): string =>
  `User ${userId.trim()} has been updated ...`;

export const userDeletedMessage = (userId: string): string =>
  `User ${userId.trim()} has been deleted ...`;
