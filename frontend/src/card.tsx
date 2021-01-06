import React from "react";
import { StatusIcon } from "@/icon.tsx";
import { Job } from "@/api.ts";

export const Card = (props: React.PropsWithChildren<{ state?: string }>) => {
  return (
    <div
      className={`bg-white ${
        props.state === "selected" ? "bg-gray-200" : ""
      } rounded shadow-md`}
    >
      <div className="px-4 py-4">{props.children}</div>
    </div>
  );
};

export const BuildInfo = ({ job }: { job: Job }) => (
  <>
    <div className="flex">
      <StatusIcon status={job.state} />
      <a
        href={`/build/${job.number}`}
        onClick={() => null}
        className="text-blue-500 mx-2"
      >
        #{job.number}
      </a>
      <div>– {job.info.message}</div>
    </div>
    <div className="build-details text-xs font-mono my-2">
      {job.info.author} pushed{" "}
      <a href={`https://github.com/${job.info.repo}/commit/${job.info.sha}`}>
        {job.info.sha}
      </a>{" "}
      to{" "}
      <a href={`https://github.com/${job.info.repo}/tree/${job.info.branch}`}>
        {job.info.branch}
      </a>
    </div>
  </>
);
