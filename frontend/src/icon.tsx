import { State } from "@/api.ts";

type StatusIconProps = {
  status: State;
};

export const StatusIcon = (props: StatusIconProps) => {
  const renderIcon = () => {
    switch (props.status) {
      case "queued":
      case "assigned":
      case "ready":
        return pending;
      case "running":
        return running;
      case "succeeded":
        return succeeded;
      case "failed":
      case "unexpectedstate":
        return failed;
      case "skipped":
        return cancelled;
    }
  };

  const background = () => {
    switch (props.status) {
      case "queued":
      case "assigned":
      case "ready":
      case "skipped":
        return "bg-gray-500";
      case "running":
        return "text-yellow-500";
      case "succeeded":
        return "bg-green-500";
      case "failed":
      case "unexpectedstate":
        return "bg-red-500";
    }
  };

  const status = () => {
    switch (props.status) {
      case "queued":
      case "assigned":
      case "ready":
        return "pending";
      case "running":
      case "succeeded":
      case "failed":
      case "unexpectedstate":
        return props.status;
    }
  };

  return (
    <div className="flex items-center">
      <div
        className={`inline-block w-6 h-6 rounded-full text-white status-${status()} ${background()}`}
      >
        {renderIcon()}
      </div>
    </div>
  );
};

export const restart = (
  <svg viewBox="0 0 24 24">
    <path d="M17.65,6.35C16.2,4.9 14.21,4 12,4A8,8 0 0,0 4,12A8,8 0 0,0 12,20C15.73,20 18.84,17.45 19.73,14H17.65C16.83,16.33 14.61,18 12,18A6,6 0 0,1 6,12A6,6 0 0,1 12,6C13.66,6 15.14,6.69 16.22,7.78L13,11H20V4L17.65,6.35Z" />
  </svg>
);

export const cancel = (
  <svg viewBox="0 0 15 15" xmlns="http://www.w3.org/2000/svg">
    <g fill="currentColor" fillRule="nonzero">
      <path d="M1.04 1.96a.65.65 0 0 1 .92-.92l12.728 12.728a.65.65 0 1 1-.92.92L1.04 1.96z" />
      <path d="M13.768 1.04a.65.65 0 0 1 .92.92L1.96 14.688a.65.65 0 1 1-.92-.92L13.768 1.04z" />
    </g>
  </svg>
);

export const cancelled = (
  <svg viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
    <g fill="none" fillRule="evenodd" transform="translate(3,3)">
      <g fill="currentColor" fillRule="nonzero">
        <path d="M7 14A7 7 0 1 1 7 0a7 7 0 0 1 0 14zm0-1.463A5.537 5.537 0 1 0 7 1.463a5.537 5.537 0 0 0 0 11.074z" />
        <path d="M2.25 3.456A.853.853 0 1 1 3.456 2.25l8.294 8.294a.853.853 0 0 1-1.206 1.206L2.25 3.456z" />
      </g>
    </g>
  </svg>
);

export const succeeded = (
  <svg
    viewBox="0 0 20 20"
    xmlns="http://www.w3.org/2000/svg"
    fillRule="evenodd"
    clipRule="evenodd"
    strokeLinejoin="round"
    strokeMiterlimit="1.414"
  >
    <path fill="none" d="M0 0h20v20H0z" />
    <path
      d="M14.577 6.23a.887.887 0 0 1 1.17-.019.704.704 0 0 1 .021 1.063l-6.844 6.439-.025.023a1.11 1.11 0 0 1-1.463-.023l-3.204-3.015a.704.704 0 0 1 .021-1.063.887.887 0 0 1 1.17.019l2.757 2.594 6.397-6.018z"
      fill="currentColor"
      fillRule="nonzero"
    />
  </svg>
);

export const failed = (
  <svg
    viewBox="0 0 20 20"
    xmlns="http://www.w3.org/2000/svg"
    fillRule="evenodd"
    clipRule="evenodd"
    strokeLinejoin="round"
    strokeMiterlimit="1.414"
  >
    <path fill="none" d="M0 0h20v20H0z" />
    <g fill="currentColor" fillRule="nonzero">
      <path d="M5.434 6.566a.802.802 0 0 1 1.132-1.132l7.778 7.778a.802.802 0 0 1-1.132 1.132L5.434 6.566z" />
      <path
        id="Line-13"
        d="M13.212 5.434a.802.802 0 0 1 1.132 1.132l-7.778 7.778a.802.802 0 0 1-1.132-1.132l7.778-7.778z"
      />
    </g>
  </svg>
);

export const pending = (
  <svg
    viewBox="0 0 20 20"
    xmlns="http://www.w3.org/2000/svg"
    fillRule="evenodd"
    clipRule="evenodd"
    strokeLinejoin="round"
    strokeMiterlimit="1.414"
  >
    <path fill="none" d="M0 0h20v20H0z" />
    <path
      d="M7.684 5.655v1.142c.001-.021.004-.018.013-.002.063.105.17.253.318.437.323.399.821.94 1.489 1.617a.92.92 0 0 1 .267.632l.018.975a.928.928 0 0 1-.287.684c-.67.639-1.167 1.156-1.489 1.544a3.55 3.55 0 0 0-.311.424c-.015.026-.017.029-.018-.009v1.246h4.632v-1.246c-.001.038-.003.035-.018.009a3.55 3.55 0 0 0-.311-.424c-.322-.388-.819-.905-1.489-1.544a.928.928 0 0 1-.287-.684l.018-.975a.92.92 0 0 1 .267-.632c.668-.677 1.166-1.218 1.489-1.617.148-.184.255-.332.318-.437.009-.016.012-.019.013.002V5.655H7.684zM6 6.884V5.138C6 4.509 6.518 4 7.158 4h5.684C13.482 4 14 4.509 14 5.138v1.746c0 .615-.616 1.401-2.092 2.911l-.007.378C13.357 11.58 14 12.389 14 13.048v1.814c0 .629-.518 1.138-1.158 1.138H7.158C6.518 16 6 15.491 6 14.862v-1.814c0-.659.643-1.468 2.099-2.875l-.007-.378C6.616 8.285 6 7.499 6 6.884z"
      fill="currentColor"
      fillRule="nonzero"
    />
  </svg>
);

export const running = (
  <svg viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
    <g fill="none" fillRule="evenodd">
      <circle cx="10" cy="10" r="5" />
      <path
        d="M5.974 2.431c.481-.257.99-.469 1.521-.63L8.095 0h3.81l.6 1.8c.53.162 1.04.374 1.521.631l1.698-.849 2.694 2.694-.85 1.698c.258.481.47.99.632 1.521l1.8.6v3.81l-1.8.6a8.518 8.518 0 0 1-.631 1.521l.849 1.698-2.694 2.694-1.698-.85c-.481.258-.99.47-1.521.632l-.6 1.8h-3.81l-.6-1.8a8.518 8.518 0 0 1-1.521-.631l-1.698.849-2.694-2.694.85-1.698a8.518 8.518 0 0 1-.632-1.521l-1.8-.6v-3.81l1.8-.6c.162-.53.374-1.04.631-1.521l-.849-1.698 2.694-2.694 1.698.85zM10 14a4 4 0 1 0 0-8 4 4 0 0 0 0 8z"
        fill="currentColor"
      />
    </g>
  </svg>
);
