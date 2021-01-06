export const endpoint = "http://localhost:9000";
export const refresh = { refreshInterval: 1000 };

// @ts-ignore
export const fetchJson = (...args) => fetch(...args).then((res) => res.json());
// @ts-ignore
export const fetchText = (...args) => fetch(...args).then((res) => res.text());

export type State =
  | "queued"
  | "assigned"
  | "ready"
  | "running"
  | "succeeded"
  | "failed"
  | "unexpectedstate"
  | "skipped";

export type Job = {
  state: State;
  number: number;
  info: {
    message: string;
    sha: string;
    author: string;
    branch: string;
    repo: string;
  };
  steps: Array<Step>;
};

export type Step = {
  name: string;
  state: State;
};
