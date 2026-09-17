/**
 * A thin Express transport over the card handlers: each route decodes a JSON
 * body into the handler request, runs the handler and returns its response.
 * No screen logic lives here.
 */

import express, { type Request, type Response, type Router } from "express";
import type { CardRecord } from "@carddemo/domain";
import type { Ksds } from "@carddemo/vsam";

import { handleCardDetail, type CardDetailRequest } from "./card-detail.js";
import { handleCardList, type CardListRequest } from "./card-list.js";
import { handleCardUpdate, type CardUpdateRequest } from "./card-update.js";

export interface CardsApiOptions {
  readonly cardFile: Ksds<CardRecord>;
  readonly now?: () => Date;
}

export const createCardsRouter = (options: CardsApiOptions): Router => {
  const router = express.Router();
  const dependencies = (): { cardFile: Ksds<CardRecord>; now?: Date } => {
    const now = options.now?.();
    return now === undefined ? { cardFile: options.cardFile } : { cardFile: options.cardFile, now };
  };

  router.post("/cards/list", (req: Request, res: Response) => {
    res.json(handleCardList(req.body as CardListRequest, dependencies()));
  });

  router.post("/cards/detail", (req: Request, res: Response) => {
    res.json(handleCardDetail(req.body as CardDetailRequest, dependencies()));
  });

  router.post("/cards/update", (req: Request, res: Response) => {
    res.json(handleCardUpdate(req.body as CardUpdateRequest, dependencies()));
  });

  return router;
};

export const createCardsApp = (options: CardsApiOptions): express.Express => {
  const app = express();
  app.use(express.json());
  app.use(createCardsRouter(options));
  return app;
};
